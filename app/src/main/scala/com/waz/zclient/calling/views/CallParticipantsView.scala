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
package com.waz.zclient.calling.views

import android.content.Context
import android.support.v7.widget.LinearLayoutManager.VERTICAL
import android.support.v7.widget.RecyclerView.ViewHolder
import android.support.v7.widget.{LinearLayoutManager, RecyclerView}
import android.util.AttributeSet
import android.view._
import android.widget.ImageView
import com.waz.ZLog.ImplicitTag._
import com.waz.model._
import com.waz.utils.events.{EventContext, Signal}
import com.waz.zclient.ViewHelper._
import com.waz.zclient.calling.controllers.CallController
import com.waz.zclient.calling.views.CallParticipantsView.CallParticipantInfo
import com.waz.zclient.common.controllers.ThemeController
import com.waz.zclient.common.views.SingleUserRowView
import com.waz.zclient.common.views.SingleUserRowView.Theme
import com.waz.zclient.common.views.SingleUserRowView.Theme.{Dark, Light, TransparentDark, TransparentLight}
import com.waz.zclient.paintcode.{ForwardNavigationIcon, GuestIconWithColor}
import com.waz.zclient.ui.text.TypefaceTextView
import com.waz.zclient.utils.ContextUtils._
import com.waz.zclient.utils.{RichView, UiStorage, UserSignal}
import com.waz.zclient.{R, ViewHelper}

class CallParticipantsView(val context: Context, val attrs: AttributeSet, val defStyleAttr: Int) extends RecyclerView(context, attrs, defStyleAttr) with ViewHelper {
  def this(context: Context, attrs: AttributeSet) = this(context, attrs, 0)
  def this(context: Context) = this(context, null)

  private implicit val uiStorage: UiStorage = inject[UiStorage]

  private val adapter = new CallParticipantsAdapter(context, inject[ThemeController], inject[CallController])

  setLayoutManager(new LinearLayoutManager(context, VERTICAL, false))
  setAdapter(adapter)
}

object CallParticipantsView {
  sealed case class CallParticipantInfo(userData: UserData, videoEnabled: Boolean)
}

class CallParticipantsAdapter(context: Context, themeController: ThemeController, callController: CallController)
                             (implicit eventContext: EventContext, uiStorage: UiStorage)
  extends RecyclerView.Adapter[ViewHolder] {
  import com.waz.zclient.calling.views.CallParticipantsAdapter._

  private var items = List.empty[CallParticipantInfo]
  private var allParticipantsNumber = 0
  private var selfTeamId = Option.empty[TeamId]
  private var theme: Theme = Theme.TransparentDark

  private lazy val maxRows = 5

  callController.callingZms.map(_.teamId).onUi { teamId =>
    selfTeamId = teamId
    notifyDataSetChanged()
  }

  callController.participantIdsToDisplay
    .map(v => v.take(maxRows - 1))
    .flatMap(users => Signal.sequence(users.map(UserSignal(_)):_*))
    .onUi { data =>
      items = data.map(u => CallParticipantInfo(u, videoEnabled = false)).toList
      //TODO: get video info
      notifyDataSetChanged()
    }

  callController.participantIdsToDisplay.map(_.size).onUi { size =>
    allParticipantsNumber = size
    notifyDataSetChanged()
  }

  Signal(callController.isVideoCall, themeController.darkThemeSet).map{
    case (true, _)      => Theme.TransparentDark
    case (false, true)  => Theme.TransparentDark
    case (false, false) => Theme.TransparentLight
  }.onUi { theme =>
    this.theme = theme
    notifyDataSetChanged()
  }

  override def getItemViewType(position: Int): Int =
    if (position == items.size) ShowAll
    else UserRow

  override def getItemCount: Int = items.size + 1

  override def getItemId(position: Int): Long =
    if (position == items.size) 0
    else items(position).userData.id.hashCode()

  setHasStableIds(true)

  override def onBindViewHolder(holder: ViewHolder, position: Int): Unit = (holder, position) match {
    case (h: CallParticipantViewHolder, pos) if pos < items.size =>
      h.bind(items(position), selfTeamId, theme)
    case (h: ShowAllButtonViewHolder, pos) if pos == items.size =>
      h.bind(allParticipantsNumber, theme)
    case _ =>
  }

  override def onCreateViewHolder(parent: ViewGroup, viewType: Int): ViewHolder = viewType match {
    case UserRow =>
      CallParticipantViewHolder(inflate[SingleUserRowView](R.layout.single_user_row, parent, addToParent = false))
    case ShowAll =>
      val view = LayoutInflater.from(parent.getContext).inflate(R.layout.list_options_button, parent, false)
      view.onClick(callController.onShowAllClick ! {})
      ShowAllButtonViewHolder(view)
  }
}

object CallParticipantsAdapter {
  val UserRow = 0
  val ShowAll = 1
}

case class CallParticipantViewHolder(view: SingleUserRowView) extends ViewHolder(view) {
  def bind(callParticipantInfo: CallParticipantInfo, teamId: Option[TeamId], theme: Theme): Unit = {
    view.setUserData(callParticipantInfo.userData, teamId, showSubtitle = false)
    view.setVideoEnabled(callParticipantInfo.videoEnabled)
    view.setTheme(theme)
    view.setSeparatorVisible(false)
  }
}

case class ShowAllButtonViewHolder(view: View) extends ViewHolder(view) {
  private implicit val ctx: Context = view.getContext
  view.findViewById[ImageView](R.id.icon).setImageDrawable(GuestIconWithColor(getStyledColor(R.attr.wirePrimaryTextColor)))
  view.findViewById[ImageView](R.id.next_indicator).setImageDrawable(ForwardNavigationIcon(R.color.light_graphite_40))

  private lazy val nameView = view.findViewById[TypefaceTextView](R.id.name_text)

  def bind(numOfParticipants: Int, theme: Theme): Unit = {
    nameView.setText(getString(R.string.show_all_participants, numOfParticipants.toString))
    setTheme(theme)
  }

  private def setTheme(theme: Theme): Unit = theme match {
    case Light =>
      nameView.setTextColor(getColor(R.color.wire__text_color_primary_light_selector))
      view.setBackgroundColor(getColor(R.color.background_light))
    case Dark =>
      nameView.setTextColor(getColor(R.color.wire__text_color_primary_dark_selector))
      view.setBackgroundColor(getColor(R.color.background_dark))
    case TransparentDark =>
      nameView.setTextColor(getColor(R.color.wire__text_color_primary_dark_selector))
      view.setBackground(getDrawable(R.drawable.selector__transparent_button))
    case TransparentLight =>
      nameView.setTextColor(getColor(R.color.wire__text_color_primary_light_selector))
      view.setBackground(getDrawable(R.drawable.selector__transparent_button))
  }
}