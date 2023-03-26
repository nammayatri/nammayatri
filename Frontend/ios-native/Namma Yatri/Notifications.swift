/* 
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

//
//  Notifications.swift
//  Namma Yatri
//
//  Created by Balaganesh on 03/02/23.
//

import UIKit

extension AppDelegate {
    
    // For notification with alert & sound
    func userNotificationCenter(_ center: UNUserNotificationCenter,
                                  willPresent notification: UNNotification,
                                  withCompletionHandler completionHandler: @escaping (UNNotificationPresentationOptions)
                                    -> Void) {
        let userInfo = notification.request.content.userInfo
        #if DEBUG
        print("userInfo: \(userInfo)")
        #endif
        let info = self.extractUserInfoFrom(dict: userInfo)
        let notificationType = info?.notificationType as? String ?? ""
        let createdAt = info?.createdAt as? String ?? ""
        if (notificationType == "REGISTRATION_APPROVED") {
            storeInHyperSDKUserDefaultsWith(key: notificationType, value: "true")
        }
        if let viewController = self.window?.rootViewController as? ViewController {
            viewController.hyperInstance.callCallback(forNotificationType: notificationType);
        }
        if (notificationType == "DRIVER_QUOTE_INCOMING" && !shouldAllowNotification(createdAt : createdAt)) {
            return;
        }
        completionHandler([[.alert, .sound]])
    }
    
    // For silent push notification
    func application(_ application: UIApplication,
                     didReceiveRemoteNotification userInfo: [AnyHashable : Any],
                     fetchCompletionHandler completionHandler: @escaping (UIBackgroundFetchResult) -> Void) {
        
        let info = self.extractUserInfoFrom(dict: userInfo)
        #if DEBUG
        print("userInfo: \(userInfo)")
        #endif
        if let viewController = self.window?.rootViewController as? ViewController {
            let notificationType = info?.notificationType as? String ?? ""
            viewController.hyperInstance.callCallback(forNotificationType: notificationType);
        }
        
        // Inform the system after the background operation is completed.
        completionHandler(.newData)
    }
    
    
    func extractUserInfoFrom(dict: [AnyHashable : Any]) -> UserInfo? {
        guard let aps = dict["aps"] as? [String: Any] else {return nil}
        guard let alert = aps["alert"] as? [String: Any] else {return nil}
        guard let data = aps["data"] as? [String: Any] else {return nil}
        guard let entityDataString = data["entity_data"] as? String else {return nil}
        guard let entityData = entityDataString.data(using: .utf8) else {return nil}
        guard let entityDataJSON = try? JSONSerialization.jsonObject(with: entityData) as? Array<Any?> else {return nil}
         
        let title = alert["title"] as? String ?? ""
        let body = alert["body"] as? String ?? ""
        let notificationType = aps["category"] as? String ?? ""
        var createdAt = ""
        if let firstEntity = entityDataJSON.first as? [String: Any] {
            createdAt = firstEntity["createdAt"] as? String ?? ""
        }
         
        let info = UserInfo(title : title, body : body, notificationType : notificationType, createdAt : createdAt)
        return info
    }
    
    // For checking the specific NotificationType: "DRIVER_QUOTE_INCOMING"
    // Returns true if notification can be allowed
    func shouldAllowNotification(createdAt: String) -> Bool{
        let createdAtDate = createdAt.replacingOccurrences(of: "T", with: " ").replacingOccurrences(of: "Z", with: "")
        let dateFormatter = DateFormatter()
        dateFormatter.dateFormat = "yyyy-MM-dd HH:mm:ss.SSSSSSSSS"
        let currentDate = dateFormatter.date(from: createdAtDate)
        
        guard let previousDate = self.previousDate else {
            self.previousDate = currentDate
            return true
        }
        
        guard let currentDate = currentDate else {
            return true
        }
        
        let diffDate = currentDate.timeIntervalSinceReferenceDate - previousDate.timeIntervalSinceReferenceDate
        if (diffDate > 30) {
            self.previousDate = currentDate
            return true
        }
        return false
    }
}
