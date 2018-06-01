/**
 * Wire
 * Copyright (C) 2018 Wire Swiss GmbH
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package com.waz.zclient.notifications.controllers

import android.app.{Notification, NotificationManager}
import android.content.Context
import android.graphics.{Color, Typeface}
import android.net.Uri
import android.os.Build
import android.support.v4.app.{NotificationCompat, RemoteInput}
import android.support.v4.app.NotificationCompat.Style
import android.text.style.{ForegroundColorSpan, StyleSpan}
import android.text.{SpannableString, Spanned}
import com.waz.ZLog.ImplicitTag.implicitLogTag
import com.waz.ZLog.warn
import com.waz.model.{ConvId, UserId}
import com.waz.utils.returning
import com.waz.utils.wrappers.Bitmap
import com.waz.zclient.Intents.{CallIntent, QuickReplyIntent}
import com.waz.zclient.utils.ContextUtils.getString
import com.waz.zclient.{Intents, R}
import com.waz.zms.NotificationsAndroidService

case class Span(style: Int, start: Int, end: Int)

object Span {
  val ForegroundColorSpanBlack = 1
  val ForegroundColorSpanGray  = 2
  val StyleSpanBold            = 3
  val StyleSpanItalic          = 4
}

case class SpannableStringBuilder(text: String, spans: List[Span] = List.empty) {
  def build: SpannableString = returning(new SpannableString(text)) { ss =>
    spans.map {
      case Span(Span.ForegroundColorSpanBlack, start, end) => (new ForegroundColorSpan(Color.BLACK), start, end)
      case Span(Span.ForegroundColorSpanGray, start, end)  => (new ForegroundColorSpan(Color.GRAY), start, end)
      case Span(Span.StyleSpanBold, start, end)            => (new StyleSpan(Typeface.BOLD), start, end)
      case Span(Span.StyleSpanItalic, start, end)          => (new StyleSpan(Typeface.ITALIC), start, end)
    }.foreach {
      case (what, start, end) => ss.setSpan(what, start, end, Spanned.SPAN_EXCLUSIVE_EXCLUSIVE)
    }
  }
}

case class StyleBuilder(style: Int,
                        title: SpannableStringBuilder,
                        summaryText: Option[String] = None,
                        bigText: Option[SpannableStringBuilder] = None,
                        lines: List[SpannableStringBuilder] = List.empty) {
  def build: Style = style match {
    case StyleBuilder.BigText =>
      returning(new NotificationCompat.BigTextStyle) { bts =>
        bts.setBigContentTitle(title.build)
        summaryText.foreach(bts.setSummaryText)
        bigText.map(_.build).foreach(bts.bigText)
      }
    case StyleBuilder.Inbox =>
      returning(new NotificationCompat.InboxStyle) { is =>
        is.setBigContentTitle(title.build)
        summaryText.foreach(is.setSummaryText)
        lines.map(_.build).foreach(is.addLine)
      }
    }
}

object StyleBuilder {
  val BigText = 1
  val Inbox   = 2
}

case class NotificationProps(when:                     Option[Long] = None,
                             showWhen:                 Option[Boolean] = None,
                             category:                 Option[String] = None,
                             priority:                 Option[Int] = None,
                             smallIcon:                Option[Int] = None,
                             contentTitle:             Option[SpannableStringBuilder] = None,
                             contentText:              Option[SpannableStringBuilder] = None,
                             style:                    Option[StyleBuilder] = None,
                             groupSummary:             Option[Boolean] = None,
                             group:                    Option[UserId] = None,
                             openAccountIntent:        Option[UserId] = None,
                             clearNotificationsIntent: Option[(UserId, Option[ConvId])] = None,
                             openConvIntent:           Option[(UserId, ConvId, Int)] = None,
                             contentInfo:              Option[String] = None,
                             color:                    Option[Int] = None,
                             vibrate:                  Option[Array[Long]] = None,
                             autoCancel:               Option[Boolean] = None,
                             sound:                    Option[Uri] = None,
                             onlyAlertOnce:            Option[Boolean] = None,
                             lights:                   Option[(Int, Int, Int)] = None,
                             largeIcon:                Option[Bitmap] = None,
                             action1:                  Option[(UserId, ConvId, Int, Boolean)] = None,
                             action2:                  Option[(UserId, ConvId, Int, Boolean)] = None
                            ) {
  def build(implicit cxt: Context): Notification = {
    val builder = new NotificationCompat.Builder(cxt, null)

    when.foreach(builder.setWhen)
    showWhen.foreach(builder.setShowWhen)
    category.foreach(builder.setCategory)
    priority.foreach(builder.setPriority)
    smallIcon.foreach(builder.setSmallIcon)
    contentTitle.map(_.build).foreach(builder.setContentTitle)
    contentText.map(_.build).foreach(builder.setContentText)
    style.map(_.build).foreach(builder.setStyle)
    groupSummary.foreach(builder.setGroupSummary)
    group.foreach(accountId => builder.setGroup(accountId.str))

    clearNotificationsIntent.foreach {
      case (accountId, Some(convId)) => builder.setDeleteIntent(NotificationsAndroidService.clearNotificationsIntent(accountId, convId, cxt))
      case (accountId, None)         => builder.setDeleteIntent(NotificationsAndroidService.clearNotificationsIntent(accountId, cxt))
    }

    openConvIntent.foreach {
      case (accountId, convId, requestBase) => builder.setContentIntent(Intents.OpenConvIntent(accountId, convId, requestBase))
    }

    contentInfo.foreach(builder.setContentInfo)
    color.foreach(builder.setColor)
    vibrate.foreach(builder.setVibrate)
    autoCancel.foreach(builder.setAutoCancel)
    sound.foreach(builder.setSound)
    onlyAlertOnce.foreach(builder.setOnlyAlertOnce)
    lights.foreach { case (c, on, off) => builder.setLights(c, on, off) }
    largeIcon.foreach(bmp => builder.setLargeIcon(bmp))

    action1.map {
      case (userId, convId, requestBase, _) =>
        new NotificationCompat.Action.Builder(R.drawable.ic_action_call, getString(R.string.notification__action__call), CallIntent(userId, convId, requestBase)).build()
    }.foreach(builder.addAction)

    action2.map {
      case (userId, convId, requestBase, bundleEnabled) => createQuickReplyAction(userId, convId, requestBase, bundleEnabled)
    }.foreach(builder.addAction)

    builder.build()
  }

  private def createQuickReplyAction(userId: UserId, convId: ConvId, requestCode: Int, bundleEnabled: Boolean)(implicit cxt: Context) = {
    if (bundleEnabled) {
      val remoteInput = new RemoteInput.Builder(NotificationsAndroidService.InstantReplyKey)
        .setLabel(getString(R.string.notification__action__reply))
        .build
      new NotificationCompat.Action.Builder(R.drawable.ic_action_reply, getString(R.string.notification__action__reply), NotificationsAndroidService.quickReplyIntent(userId, convId, cxt))
        .addRemoteInput(remoteInput)
        .setAllowGeneratedReplies(true)
        .build()
    } else
      new NotificationCompat.Action.Builder(R.drawable.ic_action_reply, getString(R.string.notification__action__reply), QuickReplyIntent(userId, convId, requestCode)).build()
  }
}

trait NotificationManagerWrapper {
  def cancel(id: Int): Unit
  def getActiveNotificationIds: Seq[Int]
  def notify(id: Int, props: NotificationProps)(implicit ctx: Context): Unit
}

object NotificationManagerWrapper {
  class AndroidNotificationsManager(notificationManager: NotificationManager) extends NotificationManagerWrapper {
    override def cancel(id: Int): Unit = notificationManager.cancel(id)

    override def getActiveNotificationIds: Seq[Int] =
      if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.M)
        notificationManager.getActiveNotifications.toSeq.map(_.getId)
      else {
        warn(s"Tried to access method getActiveNotifications from api level: ${Build.VERSION.SDK_INT}")
        Seq.empty
      }

    override def notify(id: Int, props: NotificationProps)(implicit ctx: Context): Unit =
      notificationManager.notify(id, props.build)
  }
}
