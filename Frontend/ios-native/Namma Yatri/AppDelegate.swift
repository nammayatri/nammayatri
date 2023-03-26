/* 
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

//
//  AppDelegate.swift
//  Namma Yatri
//
//  Created by Balaganesh on 20/07/22.
//

import UIKit
import FirebaseCore
import UserNotifications
import FirebaseMessaging

#if DEBUG
import FLEX
#endif

struct UserInfo{
    var title : String
    var body : String
    var notificationType : String
    var createdAt : String
}

@main
class AppDelegate: UIResponder, UIApplicationDelegate, UNUserNotificationCenterDelegate {

    var window: UIWindow?
    var previousDate: Date?

    func application(_ application: UIApplication, didFinishLaunchingWithOptions launchOptions: [UIApplication.LaunchOptionsKey: Any]?) -> Bool {
        
        UNUserNotificationCenter.current().delegate = self
        
        let authOptions: UNAuthorizationOptions = [.alert, .badge, .sound]
        UNUserNotificationCenter.current().requestAuthorization(
            options: authOptions,
            completionHandler: { _, _ in
                DispatchQueue.main.async {
                    application.registerForRemoteNotifications()
                }
            }
        )

        FirebaseApp.configure()
        Messaging.messaging().delegate = self
        
        if #available(iOS 13.0, *) {
            window?.overrideUserInterfaceStyle = .light
        }
        
        #if DEBUG
            let tapGesture = UITapGestureRecognizer.init(target: self, action: #selector(handleQuadrupleTap));
            tapGesture.numberOfTapsRequired = 4;
            self.window?.addGestureRecognizer(tapGesture);
        #endif
        
        return true
    }
    
    @objc func handleQuadrupleTap(_ tapGesture:UITapGestureRecognizer){
        
        if tapGesture.state == UIGestureRecognizer.State.recognized {
            #if DEBUG
                FLEXManager.shared.showExplorer()
            #endif
        }
    }
    
    func storeInHyperSDKUserDefaultsWith(key: String, value: Any) {
        let userDefault = UserDefaults.standard
        if let hyperSDKStore = userDefault.value(forKey: "HyperSDK") as? Dictionary<String, AnyObject> {
            let rootStore = NSMutableDictionary(dictionary: hyperSDKStore)
            rootStore.setValue(value, forKey: key)
            userDefault.setValue(rootStore, forKey: "HyperSDK")
        }
    }

}

extension AppDelegate: MessagingDelegate {
    
    // To receive FCM Token
    func messaging(_ messaging: Messaging, didReceiveRegistrationToken fcmToken: String?) {
      
        #if DEBUG
        print("Firebase registration token: \(String(describing: fcmToken))")
        #endif
        if let token = fcmToken {
            storeInHyperSDKUserDefaultsWith(key: "FCM_TOKEN", value: token)
        }
        let tokenDict = ["token": fcmToken ?? ""]
        NotificationCenter.default.post(name: Notification.Name("FCMToken"), object: nil, userInfo: tokenDict)
    }
}
