export const startPP1 = function (payload) {
	return function (sc) {
		return function () {
            console.log("hey");
			var cb = function (code) {
				return function (response) {
					return function () {
                        console.log("inside callback");
						console.log("%cHyperpay Response ","background:darkblue;color:white;font-size:13px;padding:2px", response);
						// response = castToSKDResponse(response);

                        let response1= {
                                        "payload":
                                        {
                                         "requestId":"06270f2dd1ec42b3b519e74c897b9dd6",
                                         "payload":{"status":{"value0":"charged"},
                                         "action":"paymentPage",
                                         "paymentInstrument":{"value0":"DUMMY"},
                                         "paymentInstrumentGroup":{"value0":"WALLET"},
                                         "otherInfo":{}},
                                         "service":"in.juspay.hyperpay",
                                         "error":false,
                                         "errorMessage":"",
                                         "errorCode":""},
                                         "event":"process_result"
                                        }

                                        
						sc(response1.payload.payload.status.value0)();
					}
				}
			}
			if (JOS) {
                
				try {
					// payload = JSON.parse(payload);
					// payload.pay_with_app = null;

					var outerPayload = {requestId:"61ed79dafb7545ec89fdb9b4a58107b9",
                                            service:"in.juspay.hyperpay",
                                            payload:
                                            { clientId:"nammayatri",
                                                amount:"1.18",
                                                merchantId:"nammayatri",
                                                clientAuthToken:"tkn_c5b5d61100e541b2b5da2e1db009ba79",
                                                clientAuthTokenExpiry:"2023-05-18T10:37:05Z",
                                                environment:"production",
                                                lastName:"wick",
                                                action:"paymentPage",
                                                customerId:"9876543201",
                                                currency:"INR",
                                                firstName:"john",
                                                customerPhone:"9876543201",
                                                customerEmail:"test@mail.com",
                                                orderId:"test-230732266198",
                                                description:"Order Description",
                                                return_url:"https:\/\/sandbox.juspay.in\/end"
                                            }
                                        }
                    
					console.log("%cHyperpay Request ", "background:darkblue;color:white;font-size:13px;padding:2px", outerPayload);
                    
					if (JOS.isMAppPresent("in.juspay.hyperpay")()){
                        console.log("inside process call");
                        outerPayload.service_based = "true";
						JOS.emitEvent("in.juspay.hyperpay")("onMerchantEvent")(["process",JSON.stringify(outerPayload)])(cb)();
					} else {
                        console.log("inside process call 1");
                        sc("FAIL")();
					}
				} catch (err) {
					console.error("Hyperpay Request not sent : ", err);
				}
			}else{
                sc("FAIL")();
            }
		}
	}
}