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

import java.util.concurrent.TimeUnit

import android.content.Context
import com.waz.api.IConversation
import com.waz.api.NotificationsHandler.NotificationType
import com.waz.content._
import com.waz.media.manager.context.IntensityLevel
import com.waz.model._
import com.waz.service.conversation.{ConversationsListStateService, ConversationsService, ConversationsUiService}
import com.waz.service.images.ImageLoader
import com.waz.service.push.NotificationService.NotificationInfo
import com.waz.service.push.{GlobalNotificationsService, NotificationService}
import com.waz.service.{AccountsService, MediaManagerService, UiLifeCycle, UserService}
import com.waz.specs.AndroidFreeSpec
import com.waz.testutils.TestUserPreferences
import com.waz.utils.events.{Signal, SourceSignal}
import com.waz.utils.returning
import com.waz.zclient.common.controllers.SoundController
import com.waz.zclient.common.controllers.global.AccentColorController
import com.waz.zclient.controllers.navigation.INavigationController
import com.waz.zclient.conversation.ConversationController
import com.waz.zclient.core.stores.conversation.ConversationChangeRequester
import com.waz.zclient.messages.controllers.NavigationController
import com.waz.zclient.{Module, WireContext}
import org.junit.Test
import org.scalatest.Suite
import org.threeten.bp.Instant

import scala.collection.immutable.SortedSet
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

class MessageNotificationsControllerTest extends AndroidFreeSpec { this: Suite =>
  import com.waz.ZLog.ImplicitTag.implicitLogTag

  implicit val context: WireContext = null

  class NewModule extends Module {

    implicit val timeout = Duration(5000, TimeUnit.MILLISECONDS)

    val userId = UserId()
    val convId = ConvId(userId.str)
    val convData = ConversationData(convId, RConvId(), None, userId, IConversation.Type.ONE_TO_ONE)

    // a utility method for sending infos from tests
    def sendOne(info: NotificationInfo, userId: UserId = userId) = {
      globalNotifications.groupedNotifications ! Map(userId -> (true, Seq(info)))
    }

    def send(ns: Seq[NotificationInfo], userId: UserId = userId) = {
      globalNotifications.groupedNotifications ! Map(userId -> (true, ns))
    }

    val displayedNots = new SourceSignal[Map[UserId, Seq[NotId]]]()

    val globalNotifications = new GlobalNotificationsService {
      override val groupedNotifications = Signal(Map.empty[UserId, (Boolean, Seq[NotificationService.NotificationInfo])])

      override def markAsDisplayed(userId: UserId, nots: Seq[NotId]): Future[Any] =
        Future.successful(displayedNots.mutate { cur =>
          cur + (userId -> (cur.getOrElse(userId, Seq.empty).toSet ++ nots).toSeq)
        })
    }

    val notsInManager = new SourceSignal[Map[Int, NotificationProps]]()

    private val notificationManager = new NotificationManagerWrapper {

      override def cancel(id: Int): Unit = {
        println(s"new cancel: $id")
        notsInManager ! notsInManager.currentValue.getOrElse(Map.empty).filterKeys(_ != id)
      }

      override def getActiveNotificationIds: Seq[Int] = {
        notsInManager.currentValue.getOrElse(Map.empty).keys.toSeq
      }

      override def notify(id: Int, props: NotificationProps)(implicit ctx: Context): Unit = {
        println(s"new notification: ${id -> props}")
        notsInManager ! (notsInManager.currentValue.getOrElse(Map.empty) + (id -> props))
      }
    }

/*    private val accountsStorage = returning(mock[AccountsStorage]) { accountsStorage =>
      (accountsStorage.get _).expects(account1Id).anyNumberOfTimes().returning(Future.successful(Some(accountData)))
    }*/

    private val uiLifeCycle = returning(mock[UiLifeCycle]) { uiLifeCycle =>
      (uiLifeCycle.uiActive _).expects().anyNumberOfTimes().returning(Signal.const(true))
    }

    private val usersStorage = returning(mock[UsersStorage]) { usersStorage =>
      (usersStorage.signal _).expects(userId).anyNumberOfTimes().returning(Signal.const(UserData(userId, "TestUser")))
    }

    private val convStorage = returning(mock[ConversationStorage]) { convStorage =>
      (convStorage.convsSignal _).expects().anyNumberOfTimes().returning(Signal(ConversationsSet(SortedSet(convData))))
      (convStorage.optSignal _).expects(convId).anyNumberOfTimes().returning(Signal.const(Option(convData)))
    }

    private val convsStats = returning(mock[ConversationsListStateService]) { convsStats =>
      (convsStats.selectedConversationId _).expects().anyNumberOfTimes().returning(Signal.const(Some(convId)))
      (convsStats.selectConversation _).expects(Some(convId)).anyNumberOfTimes().returning(Future.successful(()))
    }

    private val convsUi = returning(mock[ConversationsUiService]) { convsUi =>
      (convsUi.setConversationArchived _).expects(convId, false).anyNumberOfTimes().returning(Future.successful(Option(convData)))
    }

    private val conversations = returning(mock[ConversationsService]) { conversations =>
      (conversations.forceNameUpdate _).expects(convId).anyNumberOfTimes().returning(Future.successful(Option((convData, convData))))
      (conversations.isGroupConversation _).expects(convId).anyNumberOfTimes().returning(Future.successful(false))
    }

    private val imageLoader = mock[ImageLoader]
    private val mediaManager = returning(mock[MediaManagerService]) { mediaManager =>
      (mediaManager.mediaManager _).expects().anyNumberOfTimes().returning(Future.successful(null))
      (mediaManager.soundIntensity _).expects().anyNumberOfTimes().returning(Signal.const(IntensityLevel.NONE))
    }

    private val iNavController = returning(mock[INavigationController]) { iNavController =>
      (iNavController.addNavigationControllerObserver _).expects(*).anyNumberOfTimes()
    }

    private val userPrefs = returning(new TestUserPreferences) { prefs =>
      prefs.setValue(UserPreferences.VibrateEnabled, false)
      prefs.setValue(UserPreferences.RingTone, "")
      prefs.setValue(UserPreferences.TextTone, "")
      prefs.setValue(UserPreferences.PingTone, "")
    }

    // `MessageNotificationController` receives `NotificationInfo`s from here
    bind[GlobalNotificationsService] to globalNotifications
    // processed notifications end up here
    bind[NotificationManagerWrapper] to notificationManager

    // mocked global entities
    //bind[AccountsStorage] to accountsStorage
    bind[TeamsStorage]    to mock[TeamsStorage]
    bind[AccountsService] to accounts
    //(accounts.loggedInAccounts _).expects().anyNumberOfTimes().returning(Signal.const(Set(accountData)))
    bind[UiLifeCycle]     to uiLifeCycle

    // mocked services of the current ZMessaging
    //bind[Signal[AccountId]]                     to Signal.const(account1Id)
    //bind[Signal[Option[AccountId]]]             to Signal.const(Some(account1Id))
    bind[Signal[com.waz.api.AccentColor]]       to Signal.const(new com.waz.api.AccentColor { override def getColor: Int = 0 })
    bind[Signal[UsersStorage]]                  to Signal.const(usersStorage)
    bind[Signal[ConversationStorage]]           to Signal.const(convStorage)
    bind[Signal[ConversationsListStateService]] to Signal.const(convsStats)
    bind[Signal[ConversationsUiService]]        to Signal.const(mock[ConversationsUiService])
    bind[Signal[ConversationsService]]          to Signal.const(conversations)
    bind[Signal[MembersStorage]]                to Signal.const(mock[MembersStorage])
    bind[Signal[UserService]]                   to Signal.const(mock[UserService])
    bind[Signal[OtrClientsStorage]]             to Signal.const(mock[OtrClientsStorage])
    bind[Signal[AssetsStorage]]                 to Signal.const(mock[AssetsStorage])
    bind[Signal[ImageLoader]]                   to Signal.const(imageLoader)
    bind[Signal[MediaManagerService]]           to Signal.const(mediaManager)
    bind[Signal[UserPreferences]]               to Signal.const(userPrefs)

    // mocked controllers
    bind[AccentColorController]  to new AccentColorController()
    bind[NavigationController]   to new NavigationController()
    bind[INavigationController]  to iNavController

    private val convController = new ConversationController()
    convController.selectConv(Some(convId), ConversationChangeRequester.START_CONVERSATION)
    bind[ConversationController] to convController

    bind[SoundController]        to new SoundController()

  }

  feature("display notifications in one account") {
    scenario("display notification for received like") {

      implicit val module: NewModule = new NewModule

      println("Hello, world!")
      module.notsInManager ! Map.empty
      Await.result(module.notsInManager.filter(_.isEmpty).head, module.timeout)

      new MessageNotificationsController()
      module.sendOne(NotificationInfo(NotId(Uid().str), NotificationType.LIKE, Instant.now, "", module.convId))

      Await.result(module.notsInManager.filter(_.size == 1).head, module.timeout)
    }
  }

  @Test
  def hello(): Unit = {
    println("Hello, world!")
  }
  /*
    @Test
    def displayNotificationForReceivedLike(): Unit = {
      notsInManager ! Map.empty
      Await.result(notsInManager.filter(_.isEmpty).head, timeout)

      new MessageNotificationsController()
      send( NotificationInfo(NotId(Uid().str), NotificationType.LIKE, Instant.now, "", convId) )

      Await.result(notsInManager.filter(_.size == 1).head, timeout)
    }

    @Test
    def receiveConnectRequest(): Unit = {
      notsInManager ! Map.empty
      Await.result(notsInManager.filter(_.isEmpty).head, timeout)

      new MessageNotificationsController()

      send(
        NotificationInfo(
          NotId(Uid().str), CONNECT_REQUEST, Instant.now(), "", convId, Some("TestUser"),
          Some("TestUser"), userPicture = None, isEphemeral = false, hasBeenDisplayed = false
        )
      )

      Await.result(notsInManager.filter(_.size == 1).head, timeout)
    }

    override protected def info: Informer = new Informer {
      override def apply(message: String, payload: Option[Any]): Unit = println(message)
    }

    @Test
    def receiveNotificationsFromManyUsers(): Unit = {
      notsInManager ! Map.empty
      Await.result(notsInManager.filter(_.isEmpty).head, timeout)

      new MessageNotificationsController(true)

      val users = Map(UserId() -> "user1", UserId() -> "user2", UserId() -> "user3")
      val user1 = (UserId(), "user1")
      val user2 = (UserId(), "user2")
      val user3 = (UserId(), "user3")

      val ns = List(
        (user1, "1:1"),
        (user2, "2:2"),
        (user3, "3:3"),
        (user1, "1:4"),
        (user3, "3:5"),
        (user2, "2:6"),
        (user3, "3:7"),
        (user1, "1:8"),
        (user3, "3:9")
      )

      val infos = ns.map { n =>
        NotificationInfo(
          NotId(Uid().str), TEXT, Instant.now(), n._2, ConvId(n._1._1.str), Some(n._1._2),
          Some(n._1._2), userPicture = None, isEphemeral = false, hasBeenDisplayed = false
        )
      }

      send(infos, accountId)

      Await.result(notsInManager.filter(_.size == 2).head, timeout)
    }
    */
}
