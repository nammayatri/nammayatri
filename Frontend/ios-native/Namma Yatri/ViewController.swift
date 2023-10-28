/* 
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

//
//  ViewController.swift
//  Namma Yatri
//
//  Created by Balaganesh on 20/07/22.
//

import UIKit
import HyperSDK
import FirebaseAnalytics
import Firebase
import Lottie

class ViewController: UIViewController {
    
    let hyperInstance = HyperServices()
    let animationView = LottieAnimationView(name: "splash_lottie")
    let imageView: UIImageView = UIImageView.init() ;
    let footerImageView: UIImageView = UIImageView.init();
    override func viewDidLoad() {
        super.viewDidLoad()
        
//        self.imageView = UIImageView(frame: view.bounds)
        self.imageView.frame = (view.bounds);
        self.imageView.image = UIImage(named: "ny_ic_splash_bg")
        self.imageView.contentMode = .scaleAspectFill

        self.footerImageView.frame = CGRect(x: (view.bounds.width/2) - 144 ,y: view.bounds.height-120,width: 288,height: 58)
        self.footerImageView.image = UIImage(named: "ic_no_coimmission")
        self.footerImageView.contentMode = .scaleAspectFit
        view.addSubview(imageView)
        view.addSubview(footerImageView)

        animationView.frame = view.bounds
        animationView.contentMode = .scaleAspectFit
        animationView.loopMode = .loop
        animationView.play()
        view.addSubview(animationView)
        
        updateConfigUrl()
    }
    
    func updateConfigUrl() {
        if let path = Bundle.main.path(forResource: "NYConfig", ofType: "plist"),
           let plist = NSDictionary(contentsOfFile: path),
           let baseUrl = plist["baseUrl"],
           let merchantId = plist["merchantId"] {
            updatKeyInSharedPref(key: "BASE_URL", value: baseUrl)
            updatKeyInSharedPref(key: "MERCHANT_ID", value: merchantId)
           }
    }
    
    func updatKeyInSharedPref(key: String, value: Any) {
        let userDefault = UserDefaults.standard
        if let hyperSDKStore = userDefault.value(forKey: "HyperSDK") as? Dictionary<String, AnyObject> {
            let rootStore = NSMutableDictionary(dictionary: hyperSDKStore)
            rootStore.setValue(value, forKey: key)
            userDefault.setValue(rootStore, forKey: "HyperSDK")
        }
    }
    
    override func viewDidAppear(_ animated: Bool) {
        super.viewDidAppear(animated)
        self.hyperInstance.hyperDelegate = self
        if (!hyperInstance.isInitialised()) {
              initiateApp()
        }
    }
    
    func getInitiatePayload() -> [String: Any] {
        
        var initiatePayload = [String : Any]()
        initiatePayload["action"] = "initiate"
        initiatePayload["clientId"] = "nammayatri"
        initiatePayload["environment"] = "prod"
        
        if let path = Bundle.main.path(forResource: "GoogleService-Info", ofType: "plist"),
           let plist = NSDictionary(contentsOfFile: path), let apiKey = plist["API_KEY"] {
            initiatePayload["gmsAPIKey"] = apiKey
        }

        return wrapWithService("in.juspay.nammayatri", initiatePayload)
    }
    
    func getProcessPayload() -> [String: Any] {
        
        var processPayload = [String : Any]()
        processPayload["action"] = "process"
        processPayload["service"] = "in.juspay.nammayatri"
        processPayload["environment"] = "prod"

        return wrapWithService("in.juspay.nammayatri", processPayload)
    }
    
    func generateRequestId() -> String {
        var uuids = UUID().uuidString.components(separatedBy: "-")
        var i = 0
        while (i < uuids.count){
            if ( i % 2 != 0){
                uuids[i] = uuids[i].uppercased()
            }
            i += 1
        }
        return uuids.joined(separator: "-")
    }
    
    func wrapWithService(_ service: String, _ payload: [String: Any]) -> [String: Any] {
        return [
            "service": service,
            "requestId": generateRequestId(),
            "betaAssets": false,
            "payload": payload,
            "service_based": true,
            "activity_recreated": "false",
            "sdkName":"godel",
            "sdkVersion":"2.1.15"
        ]
    }
    
    func initiateApp() {
        
        let initiatePayload = getInitiatePayload()
        
        self.hyperInstance.initiate(self, payload: initiatePayload) { [unowned self] response in
            
            #if DEBUG
                print("SDK response: \(String(describing: response))")
            #endif
            
            if let data = response {
                let event = data["event"] as? String ?? ""
                if event == "initiate_result" {
                    self.hyperInstance.process(self.getProcessPayload())
                } else if event == "process_result" {
                    
                } else if event == "reboot" {
                    hyperInstance.terminate()
                    initiateApp()
                } else if event == "hide_splash" {
                    self.animationView.stop()
                    self.animationView.removeFromSuperview()
                    self.imageView.removeFromSuperview()
                } else if event == "firebase_log_event" {
                    if let data = data["data"] as? [String: Any], let name = data["name"] as? String, let params = data["parameters"] as? [String: Any] {
                        Analytics.logEvent(name, parameters: params)
                    }
                    Firebase.Analytics.setAnalyticsCollectionEnabled(true)
                }
                else if event == "firebase_user_id" {
                    if let userId = data["data"] as? String {
                        Analytics.setUserID(userId)
                    }
                    Firebase.Analytics.setAnalyticsCollectionEnabled(true)
                }
            }
        }
    }
}

// MARK: Lottie Animation delegate function
extension ViewController: HyperDelegate {
 
    func lottieAnimationView(_ fileName: String, loopEnabled: Bool, animSpeed: CGFloat) -> UIView {
        let animationView = LottieAnimationView(name: fileName)
        animationView.loopMode = loopEnabled ? .loop : .playOnce
        animationView.animationSpeed = animSpeed
        return animationView
    }
    
    func playAnimation(onLottieView view: UIView) {
        if let animationView = view as? LottieAnimationView {
            animationView.play()
        }
    }
    
    func stopAnimation(onLottieView view: UIView) {
        if let animationView = view as? LottieAnimationView {
            animationView.stop()
        }
    }
}
