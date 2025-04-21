window.config_version = "2.0.52";

JBridge.trackEvent("config_version" , ""+window.config_version );

if (window.JOS && typeof window.JOS.registerVersion == "function"){
    window.JOS.registerVersion(window.JOS.self)("config")(window.config_version)();
}

var ALL = "true";
if (typeof clientId == "undefined") {
    if (window.__payload && (window.__payload.client_id || window.__payload.clientId)) {
        var clientId = window.__payload.client_id || window.__payload.clientId;
    } else if (window.__payload && window.__payload.payload && (window.__payload.payload.client_id || window.__payload.payload.clientId)) {
        var clientId = window.__payload.payload.client_id || window.__payload.payload.clientId;
    } else {
        var clientId = "default"
    }
}

console.log("clientId for upi-intent config: " + clientId);

var addToFirst = function(first, second) {
	for(var key in second) {
		first[key] = second[key];
	}
	return first;
}

var match = function(params, returnOnFirstMatch) {
	var config = {};
	console.log("params: " + JSON.stringify(params));
	for(var i in params) {
		var entry = params[i];
		var condition = entry.c;
		var result = entry.r;
		console.log("Condition: " + condition+", Result: "+JSON.stringify(result));
		try {
			if(eval(condition)) {
				console.log("condition is true for : " + JSON.stringify(entry));
				config = addToFirst(config, result);
				if(returnOnFirstMatch) {
					return config;
				}
			}
		} catch(e) {
			console.error("Error when evaluating condition: " + condition + ". Exception: " + e);
		}
	}
	console.log("Returning config ")
	return config;
}

var matchAll = function(params) {
	return match(params, false);
}

var matchFirst = function(params) {
	return match(params, true);
}

var defaultStrings = {
    "PLEASE_WAIT": "Please wait",
    "CANCEL": "CANCEL",
    "REFRESH": "REFRESH",
    "REFRESHING": "Refreshing...",
    "SHOW_QR": "SHOW QR",
    "LOADING_REFRESH": "REFRESH",
    "TAKING_LONG": "This is taking too long...",
    "MEANWHILE": "Meanwhile, you can also try completing the payment using a QR code",
    "WAITING_CONFIRMATION": "Waiting for confirmation...",
    "CONFIRMING_PAYMENT": "Confirming your payment...",
    "GENERATING_QR_CODE": "Generating QR code...",
    "INITIATING_COLLECT": "Initiating collect request...",
    "INITIATING_TRANSACTION": "Initiating transaction...",
    "GO_TO_UPI": "Please go to your UPI app and complete the payment...",
    "GENERATE_QR": "Generate QR",
    "CHOOSE_UPI_APP": "Choose a UPI App",
    "OR": "OR",
    "ENTER_UPI_ID": "Enter UPI ID",
    "REQUEST_NOTIFICATION": "A collect request notification will be sent to this UPI ID",
    "SAVE_FOR_FUTURE": "Save for future",
    "SAVED_UPI_ID": "Saved UPI ID",
    "WAITING_PAYMENT_CONFIRMATION": "Waiting for payment confirmation...",
    "COLLECT_USING_UPI_ID": "COLLECT USING UPI ID",
    "COLLECTING_FROM": "COLLECTING FROM",
    "RUPEE_SYMBOL": "₹ " ,
    "ORDER_ID": "Order ID" ,
    "QR_REFRESH": "REFRESH",
    "CUSTOMER_S_UPI_ID": "CUSTOMER’S UPI ID",
    "VPA_HINT": "example@upi",
    "LOADING_TXN_PLEASE_WAIT": "Please Wait",
    "LOADINGSCREEN_SWITCH_APP_TEXT": "Redirecting...",
    "LOADINGSCREEN_SWITCH_APP_SUB_TEXT": "",
    "LOADING_ORDER_STATUS_TEXT": "Please Wait",
    "LOADING_ORDER_STATUS_SUB_TEXT": "Redirecting you back to merchant application...",
    "LOADING_COLLECT_TEXT": "Waiting for payment...",
    "LOADING_COLLECT_SUB_TEXT": "Please go to your UPI app and complete the payment",
    "LOADING_COLLECT_WITHQR_TEXT": "Waiting for payment...",
    "LOADING_COLLECT_WITHQR_SUB_TEXT": "Please go to your UPI app and complete the payment",
    "LOADING_COLLECT_WITHQR_LAYOUT": "This is taking too long...",
    "LOADING_COLLECT_WITHQR_LAYOUT_SUB_TEXT": "Meanwhile you can also try completing the payment using a QR code.",
    "LOADINGSCREEN_REDIRECTING_TEXT": "Redirecting to merchant..",
    "LOADINGSCREEN_REDIRECTING_SUB_TEXT": "",
    "DESCRIPTION": "Description",
    "TRANSACTION_ID": "Trasaction Id",
    "PAYMENT_SUCCESSFULL": "Payment successful!",
    "PAYMENT_FAILED": "Payment failed!",
    "YOU_PAID": "You Paid",
    "REQUEST_AMOUNT": "REQUEST AMOUNT",
    "PLEASE_CONNECT_TO_MAKE_REQUEST": "Please connect to network to make the Request!",
    "NO_NETWORK_CONNECTION": "No network connection!",
    "RECONNECTING": "Reconnecting...",
    "PRESSBACK_TO_CANCEL": "Press BACK again to cancel the transaction"
}

var getStrings = function() {
    var s = defaultStrings;
    switch(clientId) {
        case "Grofers_UPI_ID":
            s = defaultStrings;
            break;
        default:
            s = defaultStrings;
            break;
    }
    return s;
}

//try to get gateway_id from bundle
var getGatewayId = function() {
    try {
        if(window.__payload) {
            return window.__payload.gateway_id;
        } else {
            var bundle = JSON.parse(JBridge.getSessionAttribute("bundleParams", "{}"));
            return bundle.gateway_id;
        }
    } catch(e) {
        console.error(e);
        return null;
    }
}

var getPriorityApps = function() {
    var priorityApps = [];
    switch(clientId) {
        case "dreamplug_android":
            priorityApps = ["com.google.android.apps.nbu.paisa.user", "com.phonepe.app", "net.one97.paytm"]
            break;
        case "Grofers_UPI_ID":
            priorityApps = ["net.one97.paytm", "com.mobikwik_new", "com.google.android.apps.nbu.paisa.user", "com.phonepe.app", "in.org.npci.upiapp"];
            break;
        case "mplgaming":
            priorityApps = ["net.one97.paytm"]
            break;
        case "playo_android":
            priorityApps = ["net.one97.paytm"]
            break;
        case "orixindia_android":
            priorityApps = ["net.one97.paytm"]
            break;
        case "ixigo_android":
        case "ixigo_ANDROID":
        case "ixigoprod_ANDROID":
        case "ixigoprod_android":
            priorityApps = ["com.phonepe.app", "com.google.android.apps.nbu.paisa.user", "net.one97.paytm", "in.amazon.mShop.android.shopping", "com.csam.icici.bank.imobile", "com.enstage.wibmo.hdfc", "com.upi.axispay", "com.freecharge.android", "com.mobikwik_new", "com.samsung.android.spay"]
            break;
        case "jiosaavn_android":
            priorityApps = ["net.one97.paytm"];
            break;
        case "idea_android":
        case "vodafone_android":
        case "vodaidea_android":
            priorityApps = ["net.one97.paytm", "com.google.android.apps.nbu.paisa.user", "in.org.npci.upiapp", "com.phonepe.app", "com.mobikwik_new"];
            break;
        case "zee5_android":
            priorityApps = ["com.google.android.apps.nbu.paisa.user", "net.one97.paytm", "com.phonepe.app", "in.amazon.mShop.android.shopping", "in.org.npci.upiapp"];
            break;
        case "TOI_android":
            priorityApps = ["com.google.android.apps.nbu.paisa.user", "com.phonepe.app","net.one97.paytm", "in.org.npci.upiapp", "in.amazon.mShop.android.shopping", "com.csam.icici.bank.imobile", "com.sbi.upi"];
            break;
        case "astroyogi_android":
            priorityApps = ["com.google.android.apps.nbu.paisa.user", "com.phonepe.app", "in.org.npci.upiapp", "net.one97.paytm", "in.amazon.mShop.android.shopping"];
            break;
        default:
            priorityApps = ["com.google.android.apps.nbu.paisa.user", "in.org.npci.upiapp", "com.phonepe.app"];
            break;
    }

    return priorityApps;
}

var getPollTimings = function() {
	var poll_timing = {};
	switch(clientId) {
		case "Grofers_UPI_ID":
			poll_timing = { interval: 5000, no_of_retries: 6, max_time: 30000 }
			break;
		default:
			poll_timing = { interval: 5000, no_of_retries: 6, max_time: 30000 }
			break;
	}
	return poll_timing;
}

var getRedirectTiming = function() {
    var redirect_timing;
    switch(clientId) {
        case "Grofers_UPI_ID":
            redirect_timing = 2000;
            break;
        default:
            redirect_timing = 2000;
            break;
    }
    return redirect_timing;
}

var getAllowedFlows = function(flow) {
    var allowed_flows = [];
    if(clientId == "Grofers_UPI_ID") {
        switch(flow) {
            case "collect":
                allowed_flows = ["dynamicqr"]; break;
        };
    } else if(clientId == "zinka_android" || clientId == "zinka_commerce_android") {
        allowed_flows = [];
    } else if(clientId == "playo_android") {
        allowed_flows = [];
    } else if (clientId == "fanfight_android") {
        allowed_flows = [];
    } else if (clientId == "gaana_android") {
        switch (flow) {
            case "collect":
                allowed_flows = ["intent"]; break;
        };
    } else if (clientId == "netmeds_android") {
        allowed_flows = [];
    } else if (clientId == "orixindia_android") {
        allowed_flows = [];
    } else if (clientId == "curefit_android") {
        allowed_flows = [];
    } else if (clientId == "Curefit_android") {
        allowed_flows = [];
    } else if (clientId == "grofers_android") {
        allowed_flows = [];
    } else if (clientId == "vodafone_android") {
        allowed_flows = [];
    } else {
        switch(flow) {
            case "collect":
                allowed_flows = ["intent", "dynamicqr"]; break;
        };
    }

    return allowed_flows;
}

var getOverriddenProperties = function () {
    var iconUrls = {
        "in.amazon.mShop.android.shopping" : {iconUrl : "https://assets.juspay.in/hyper/images/internal/ic_amazonpay.png"},
        "net.one97.paytm" : {iconUrl : "https://assets.juspay.in/hyper/images/internal/ic_paytm_allinone.png"}
    }
    return iconUrls;
}

var getFilterList = function() {
    var appList = {filter_type: "blacklist", list: []}

    if(clientId === "Grofers_UPI_ID") {
        appList = {filter_type: "whitelist", list: ["net.one97.paytm", "com.phonepe.app", "com.google.android.apps.nbu.paisa.user", "com.mobikwik_new", "com.csam.icici.bank.imobile", "in.org.npci.upiapp", "in.amazon.mShop.android.shopping", "com.whatsapp", "com.freecharge.android"]}
    } else if(clientId === "grofers_android") {
        appList = {filter_type: "whitelist", list: ["net.one97.paytm", "com.google.android.apps.nbu.paisa.user", "com.upi.axispay", "com.phonepe.app", "com.csam.icici.bank.imobile", "in.org.npci.upiapp", "com.mobikwik_new", "com.freecharge.android","com.sbi.upi", "com.bankofbaroda.upi", "com.fss.idfcpsp", "com.icicibank.pockets", "com.mgs.obcbank", "com.fss.pnbpsp", "com.airtel.abpl", "com.myairtelapp", "com.fss.unbipsp", "com.icici.eazypaycollect", "in.amazon.mShop.android.shopping", "com.fedmobile", "axis.fastacash.com.pingpay_android"]}
    }else if(clientId === "olacabs_android" || clientId === "olamoney_android") {
        appList = {filter_type: "whitelist", list: [ "in.cointab.app", "com.myairtelapp", "com.google.android.apps.nbu.paisa.user", "com.phonepe.app", "in.org.npci.upiapp" , "net.one97.paytm" , "com.sbi.upi" ]}
    } else if(clientId === "olacabs_sandbox_android") {
        appList = {filter_type: "whitelist", list: [ "in.cointab.app", "com.myairtelapp", "com.google.android.apps.nbu.paisa.user", "com.phonepe.app", "in.org.npci.upiapp" , "net.one97.paytm" , "com.sbi.upi" ]}
    } else if(clientId === "dreamplug_android") {
        appList = {filter_type: "blacklist", list: ["com.olacabs.customer", "com.csam.icici.bank.imobile", "com.dbs.in.digitalbank", "com.bankofbaroda.upi", "com.icicibank.pockets", "com.axis.mobile", "com.upi.axispay", "com.myairtelapp", "com.snapwork.hdfc"]}
    } else if(clientId == "bigbasket_android" || clientId == "bb_android" || clientId == "bb_staging_android" || clientId == "bigbasket") {
        appList = {filter_type: "whitelist", list: ["com.google.android.apps.nbu.paisa.user", "in.org.npci.upiapp", "net.one97.paytm", "com.upi.axispay", "com.snapwork.hdfcbank", "com.csam.icici.bank.imobile", "com.olive.kotak.upi"]}
    } else if (clientId == "ixigoprod_ANDROID" || clientId == "ixigo_ANDROID" || clientId == "ixigo_android" || clientId == "ixigoprod_android") {
        appList = {filter_type: "whitelist", list: ["com.phonepe.app", "com.google.android.apps.nbu.paisa.user", "net.one97.paytm", "in.amazon.mShop.android.shopping", "com.csam.icici.bank.imobile", "com.enstage.wibmo.hdfc", "com.upi.axispay", "com.freecharge.android", "com.mobikwik_new", "com.samsung.android.spay"]}
    } else if(clientId == "housejoy_android") {
        appList = {filter_type: "whitelist", list: ["com.google.android.apps.nbu.paisa.user", "com.phonepe.app", "net.one97.paytm", "com.truecaller"]}
    } else if(clientId == "zinka_android" || clientId == "zinka_commerce_android") {
        appList = {filter_type: "blacklist", list: ["com.olacabs.customer", "com.fss.pnbpsp", "com.whatsapp", "com.upi.axispay", "com.axis.mobile", "com.myairtelapp", "in.amazon.mShop.android.shopping", "com.snapwork.hdfc", "com.snapwork.hdfcbank", "com.lenovo.anyshare.gps", "com.enstage.wibmo.hdfc", "com.gbwhatsapp", "com.infra.boiupi", "com.kvb.mobilebanking", "com.lcode.allahabadupi", "com.freecharge.android", "com.dbs.in.digitalbank", "com.atomyes", "com.canarabank.mobility", "com.infrasoft.uboi", "com.CholaCFA", "com.rblbank.mobank", "com.infrasofttech.centralbankupi", "com.mipay.wallet.in", "com.dreamplug.androidapp", "com.lcode.smartz", "com.ultracash.payment.customer", "com.icicibank.pockets", "com.fss.unbipsp", "in.epaylater.android.consumer", "com.finopaytech.bpayfino", "com.svs.shriramcity", "com.YesBank", "net.one97.paytn", "net.one97.paytm", "com.abipbl.upi", "com.fss.ubipsp", "com.lcode.ucoupi", "com.fss.jnkpsp", "in.juspay.amazonpay", "com.justdial.search", "com.mycompany.kvb", "com.mgs.induspsp", "com.fss.idfcpsp", "com.truecaller"]}
    } else if(clientId == "dream11_android") {
        appList = {filter_type: "whitelist", list: ["com.phonepe.app", "com.google.android.apps.nbu.paisa.user", "net.one97.paytm", "in.org.npci.upiapp", "com.truecaller", "in.amazon.mshop.android.shopping", "com.sbi.upi"]}
    } else if(clientId == "playo_android") {
        appList = {filter_type: "blacklist", list: ["com.olacabs.customer", "com.csam.icici.bank.imobile"]}
    } else if(clientId == "foodpanda_android") {
        appList = {filter_type: "blacklist", list: ["com.olacabs.customer", "com.csam.icici.bank.imobile"]}
    } else if(clientId == "railyatri_android") {
        appList = {filter_type: "blacklist", list: ["com.olacabs.customer", "com.axis.mobile", "in.org.npci.upiapp", "com.whatsapp", "com.sbi.upi", "com.infrasoft.uboi", "com.makemytrip"]}
    } else if (clientId == "idea_android" ) {
        appList = {filter_type: "blacklist", list: ["com.whatsapp", "com.citrus.citruspay", "com.snapwork.hdfc", "com.myairtelapp", "com.lenovo.anyshare.gps", "com.axis.mobile", "com.infrasoft.uboi", "com.canarabank.mobility", "com.snapwork.hdfcbank", "com.enstage.wibmo.hdfc", "com.dbs.in.digitalbank", "com.gbwhatsapp", "com.atomyes", "com.kvb.mobilebanking", "in.epaylater.android.consumer", "com.corpay.mwallet", "com.icicibank.pockets", "com.SIBMobile", "com.makemytrip", "com.fmwhatsapp", "in.bajajfinservmarkets.app", "com.yowhatsapp", "com.rblbank.mobank", "com.lcode.allahabadupi", "com.infrasofttech.centralbankupi", "com.jio.myjio", "com.infrasofttech.mahaupi", "com.justdial.search", "com.fss.unbipsp", "com.fss.indus", "com.cub.wallet.gui", "com.olacabs.customer", "com.msf.angelmobile", "com.udaan.android", "com.daamitt.walnut.app", "com.olive.kotak.upi", "com.finacus.jetpay", "com.ultracash.payment.customer", "com.lcode.csbupi", "com.idbibank.paywiz", "com.fisglobal.bandhanupi.app", "com.mgs.hsbcupi", "com.lcode.corpupi", "com.aero", "com.euronet.iobupi", "com.mgs.induspsp", "com.lcode.dlbupi", "net.one97.paytq", "upi.npst.com.upicanara", "in.scanpay.app", "com.mipay.in.wallet", "com.sc.scbupi", "com.jtwhatsapp", "com.gbwhatsapp3", "in.chillr", "com.lcode.ucoupi", "com.abipbl.upi", "com.bsb.hike", "com.infra.boiupi", "com.svs.shriramcity", "com.freecharge.android", "com.truecaller", "com.mipay.wallet.in", "com.csam.icici.bank.imobile"]}
    } else if (clientId == "vodafone_android" || clientId == "vodaidea_android" ) {
        appList = {filter_type: "blacklist", list: ["com.whatsapp", "com.citrus.citruspay", "com.snapwork.hdfc", "com.myairtelapp", "com.lenovo.anyshare.gps", "com.axis.mobile", "com.infrasoft.uboi", "com.canarabank.mobility", "com.snapwork.hdfcbank", "com.enstage.wibmo.hdfc", "com.dbs.in.digitalbank", "com.gbwhatsapp", "com.atomyes", "com.kvb.mobilebanking", "in.epaylater.android.consumer", "com.corpay.mwallet", "com.icicibank.pockets", "com.SIBMobile", "com.makemytrip", "com.fmwhatsapp", "in.bajajfinservmarkets.app", "com.yowhatsapp", "com.rblbank.mobank", "com.lcode.allahabadupi", "com.infrasofttech.centralbankupi", "com.jio.myjio", "com.infrasofttech.mahaupi", "com.justdial.search", "com.fss.unbipsp", "com.fss.indus", "com.cub.wallet.gui", "com.olacabs.customer", "com.msf.angelmobile", "com.udaan.android", "com.daamitt.walnut.app", "com.olive.kotak.upi", "com.finacus.jetpay", "com.ultracash.payment.customer", "com.lcode.csbupi", "com.idbibank.paywiz", "com.fisglobal.bandhanupi.app", "com.mgs.hsbcupi", "com.lcode.corpupi", "com.aero", "com.euronet.iobupi", "com.mgs.induspsp", "com.lcode.dlbupi", "net.one97.paytq", "upi.npst.com.upicanara", "in.scanpay.app", "com.mipay.in.wallet", "com.sc.scbupi", "com.jtwhatsapp", "com.gbwhatsapp3", "in.chillr", "com.lcode.ucoupi", "com.abipbl.upi", "com.bsb.hike", "com.infra.boiupi", "com.svs.shriramcity", "com.freecharge.android", "com.truecaller", "com.mipay.wallet.in", "com.csam.icici.bank.imobile", "com.sharekaro.app"]}
    } else if (clientId == "jiosaavn_android") {
        appList = {filter_type: "whitelist", list: ["com.google.android.apps.nbu.paisa.user", "com.phonepe.app", "net.one97.paytm", "in.org.npci.upiapp", "in.amazon.mShop.android.shopping", "com.freecharge.android"]}
    } else if (clientId == "urbanclap_android") {
        appList = {filter_type : "whitelist", list: ["com.google.android.apps.nbu.paisa.user", "com.phonepe.app","net.one97.paytm", "in.org.npci.upiapp"] }
    } else if (clientId == "onecard_android" ) {
        appList = {filter_type: "whitelist", list: ["com.phonepe.app", "com.google.android.apps.nbu.paisa.user", "in.amazon.mShop.android.shopping", "in.org.npci.upiapp", "net.one97.paytm"]}    
    } else if (clientId == "gaana_android") {
        appList = {filter_type: "blacklist", list: ["com.csam.icici.bank.imobile", "com.whatsapp", "com.lenovo.anyshare.gps"]}
    } else if (clientId == "okcredit_android") {
        appList = {filter_type: "whitelist", list: ["com.google.android.apps.nbu.paisa.user", "com.phonepe.app", "net.one97.paytm", "in.amazon.mShop.android.shopping", "in.org.npci.upiapp"]}
    } else if (clientId == "zee5_android") {
        appList = {filter_type: "whitelist", list: ["com.google.android.apps.nbu.paisa.user", "com.phonepe.app", "net.one97.paytm", "in.org.npci.upiapp", "com.icicibank.pockets", "in.amazon.mShop.android.shopping", "com.upi.axispay"]}
    } else if (clientId == "truefan_android") {
        appList = {filter_type: "whitelist", list: ["com.google.android.apps.nbu.paisa.user", "com.phonepe.app", "net.one97.paytm", "in.org.npci.upiapp", "in.amazon.mShop.android.shopping"]}
    } else if (clientId == "TOI_android") {
        appList = {filter_type : "whitelist", list: ["com.google.android.apps.nbu.paisa.user", "com.phonepe.app","net.one97.paytm", "in.org.npci.upiapp", "com.csam.icici.bank.imobile", "com.sbi.upi"]}
    } else if (clientId == "firstcry_android") {
        appList = {filter_type: "whitelist", list: ["com.google.android.apps.nbu.paisa.user", "com.phonepe.app", "net.one97.paytm", "in.amazon.mShop.android.shopping"]}
    } else if (clientId === "trulymadly_android") {
        appList = {filter_type: "whitelist", list: ["net.one97.paytm", "com.phonepe.app", "in.amazon.mShop.android.shopping", "com.google.android.apps.nbu.paisa.user", "in.org.npci.upiapp"]}
    } else {
        appList = {filter_type: "blacklist", list: ["com.olacabs.customer"]}
    }

    return appList;
}

var getMandateFilterList = function() {
    var appList = getFilterList();
    var mandateAppList = {filter_type: appList.filter_type, list: appList.list};

    if (clientId === "jiosaavn_android") {
        mandateAppList = { filter_type: "whitelist", list: ["net.one97.paytm", "in.org.npci.upiapp"] };
    } else if (clientId == "gaana_android") {
        mandateAppList = { filter_type: "whitelist", list: ["net.one97.paytm", "in.org.npci.upiapp"]}
    }

    console.log("Mandate filter list is: ", mandateAppList);
    return mandateAppList;
}

var getTidPrefix = function() {
    var tidPrefix;

    switch(clientId) {
        case "foodpanda_android":
            tidPrefix = "AXISSDKV3";
            break;
        case "olacabs_android":
            tidPrefix = "AXISSDKV3";
            break;
        case "olacabs_sandbox_android":
            tidPrefix = "AXISSDKV3";
            break;
        default:
            tidPrefix = "";
    }

    return tidPrefix;
}

var isDreamplugBeta = function() {
    try {
        eval(JBridge.getConfigVariables());
        var dreamplugBetaUsers = [
            "8568d67a13b022435334db188e9dbdbe9676d7c665581989fd7f5854d165afd6",
            "e9fca297d2f05b95b0c701188012ed7e8798af26ea7eb07bc2153bd498514317",
            "6e5ed80461e1f7bd88c918d877b032a739eb3f77f620164d5aa8adb14d3d4bcb",
            "0706ea2986b65f0e2ae6fd3471d5b589da5c4d92601c251a7b431abe09314d6f",
            "2facff58598c76eb43d931b456f93420dd5e3dab146384e4f2f6726b007e3320",
            "2ab5d9207782f236779f20ece92.0.528867ef8306b4bcd63658122cf33cdfbe",
            "2da822e5a7ea742ed76988f74102003e1e18f60097e1b065e24a1931190cbe41",
            "beaf260b96c8ea0d3d9279e3c7d6101698122661108860c0759e080c8edfc7f9",
            "09e25a2558bad6dc298b206bb740598c424c9b56a1d979bfb88898394db10e59",
            "e06c3bc1990485a1bea4cb4c1ab677e8e1e4b17449bde1f0f326bc6519a57e3f",
            "d078d2b6f9343b0fe64d4247980a8c4baecb154a7a9fae55e4fec4ba7710d861",
            "8c291913067979fa43b15bd29dd6cf78aaa5b143a17214793e7d6dd1ca3b350c",
            "883d060fd3c2d3aeadf3b7d3313fede25831c5f179fc8a2ddb37d6030f9be0d6"
        ];
        for(var i = 0; i < dreamplugBetaUsers.length; i++) {
            if(juspayDeviceId == dreamplugBetaUsers[i]) {
                return true;
            }
        }

        return false;
    } catch(e) {
        return false;
    }
}

var getTxnRefLogic = function() {
    var txnRefLogic;

    if(clientId == "Grofers_UPI_ID") {
        txnRefLogic = "TXN_ID"
    } else if(clientId == "dreamplug_android") {
        txnRefLogic = "TXN_UUID";
    } else if(clientId == "ixigo_ANDROID" || clientId == "ixigoprod_ANDROID") {
        txnRefLogic = "TXN_UUID";
    } else {
        txnRefLogic = "TXN_ID"
    }

    return txnRefLogic
}

var getExitOn = function() {
    var exitOn;

    switch(clientId) {
        case "Grofers_UPI_ID":
            exitOn = {
                "failed": true,
                "submitted": true
            }
            break;
        case "grofers_android":
            exitOn = {
                "failed": true,
                "submitted": true
            }
            break;
        case "zinka_android":
            exitOn = {
                "failed": true,
                "submitted": true
            }
            break;
        case "zinka_commerce_android":
            exitOn = {
                "failed": true,
                "submitted": true
            }
            break;
        case "mpl_test":
            exitOn = {
                "failed": true,
                "submitted": true
            }
            break;
        case "dream11_android":
            exitOn = {
                "failed": true,
                "submitted": true
            }
            break;
        case "ixigo_ANDROID":
            exitOn = {
                "failed": true,
                "submitted": true
            }
            break;
        case "ixigoprod_ANDROID":
            exitOn = {
                "failed": true,
                "submitted": true
            }
            break;
        case "orixindia_android":
            exitOn = {
                "failed": true,
                "submitted": true
            }
            break;
        case "gaana_android":
            exitOn = {
                "failed": true,
                "submitted": true
            }
            break;
        default:
            exitOn = {
                "failed": false,
                "submitted": false
            }
    }

    return exitOn;
}

var getAnalyticsEndPoint = function() {
    return "https://logger.in-west-1.juspay.in/logs?twoLevel=true";
}

var getOrderCancelMaxTime = function() {
    return 5000;
}

var getGlobalConfig = function() {return matchAll(
	{
		"default_config": {
			"c": ALL,
			"r": {
                    "priorityApps": getPriorityApps(),
                    "filter_list_mandate": getMandateFilterList(),
                    "getOverriddenProperties" : getOverriddenProperties(),
					"filter_list" : getFilterList(),
					"poll_timing": getPollTimings(),
					"redirect_timing": getRedirectTiming(),
					"cache_invalidation_timeout": 24 * 60 * 60 * 1000,
					"intent": {
						"allow_from": getAllowedFlows("intent")
					},
					"collect": {
						"allow_from": getAllowedFlows("collect")
					},
					"dynamicqr" : {
					    "allow_from": getAllowedFlows("dynamicqr")
					},
					"strings": getStrings(),
					"analytics_endpoint": getAnalyticsEndPoint(),
					"order_cancel_max_time": getOrderCancelMaxTime(),
					"txnRefLogic": getTxnRefLogic(),
					"tidPrefix": getTidPrefix(),
					"exitOn": getExitOn()
			}
		}
	}
)}

var getWeblabConfig = function() {return matchAll(
	{
		"default_weblab_rules": {
			c: ALL,
			r: {
				"ENABLE_CACHE_INVALIDATION":1
			}
		}
	}
)}

var getConfigList = function() {
	var configList = [
		{"global_config": getGlobalConfig() },
		{"weblab_config": getWeblabConfig() }
	];
	var config = {};
	for(var i = 0; i < configList.length; i++) {
		addToFirst(config, configList[i]);
	}

	return config;
}

window.getConfig = function() {
	var configList = getConfigList();
	return JSON.stringify(configList);
};

var payload = {
    fileName: 'payments/in.juspay.upiintent/v1-config.jsa'
}
JBridge.runInJuspayBrowser("onBundleLoaded", JSON.stringify(payload), null);

//var config = {
//	merchant: {
//		dynamicqr: {
//			allow_from: []
//		},
//		intent: {
//			allow_from: []
//		},
//		collect: {
//			allow_from: ["intent", "dynamicqr"]
//		}
//	},
//	priorityApps: [
//	  "in.org.npci.upiapp"
//	],
//	poll_timing: {
//		interval: 5000,
//		no_of_retries: 6
//	},
//	cache_invalidation: {
//		timeout: 24 * 60 * 60 * 1000
//	}
//};

