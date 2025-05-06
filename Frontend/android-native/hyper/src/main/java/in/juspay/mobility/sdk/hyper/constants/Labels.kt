package `in`.juspay.mobility.sdk.hyper.constants

import androidx.annotation.Keep

object Labels {
    @Keep
    object HyperSdk {
        const val HYPER_SERVICE = "hyper_service"
        const val PREFETCH = "prefetch"
        const val INITIATE = "initiate"
        const val PROCESS = "process"
        const val EVENT = "on_event"
        const val JP_CONSUMING_BACK_PRESS = "jp_consuming_backpress"
        const val TERMINATE = "terminate"
        const val TERMINATE_PROCESS = "terminate_process"
        const val JUSPAY_SERVICES = "juspay_services"
        const val FIRST_TIME_SETUP = "first_time_setup"
        const val ON_DUI_READY = "on_dui_ready"
        const val ON_HTML_READY = "on_html_ready"
        const val ON_SCRIPT_ERROR = "on_script_error"
        const val ON_JOS_READY = "on_jos_ready"
        const val DUI_READY = "dui_ready"
        const val MYSTIQUE = "mystique"
        const val AUTO_FALLBACK = "auto_fallback"
        const val RUN_IN_JUSPAY_BROWSER = "run_in_juspay_browser"
        const val EXIT_SDK_ERROR = "exit_sdk_error"
        const val EXCEPTION_HANDLER = "hyper_exception_handler"
        const val PROCESS_WAIT_QUEUE = "process_wait_queue"
        const val ADD_BRIDGE = "add_bridge"
        const val SET_BUNDLE_PARAMS = "set_bundle_parameters"
        const val ON_BACKPRESSED_CALLBACK = "on_back_pressed_callback"
    }

    object Android {
        const val BACK_PRESSED = "on_back_pressed"
        const val ON_EVENT = "on_event"
        const val ON_ACTIVITY_RESULT = "on_activity_result"
        const val START_INTENT_SENDER_FOR_RESULT = "start_intent_sender_for_result"
        const val FRAGMENT_OPERATION = "fragment_operation"
        const val WEBVIEW = "webview"
    }

    object SDK {
        const val RECEIVER_CALLBACK = "receiver_callback"
        const val AMAZON_UTILS = "amazon_utils"
        const val PAYPAL_UTILS = "paypal_utils"
        const val CUSTOM_TAB = "custom_tab"
        const val PAYTM_UTILS = "paytm_utils"
        const val WEBVIEW_CLIENT = "webview_client"
    }

    object System {
        const val SDK_CRASHED = "sdk_crashed"
        const val ON_RENDER_PROCESS_GONE = "on_render_process_gone"
        const val INITIALISE_JUSPAY_WEBVIEW = "initialise_juspay_webview"
        const val LOAD_PAGE = "load_page"
        const val ADD_WEBVIEW = "add_web_view"
        const val ON_DUI_RELEASED = "on_dui_released"
        const val ON_REQUEST_PERMISSION_RESULT = "on_request_permission_result"
        const val ON_WEBVIEW_READY = "on_webview_ready"
        const val SHARED_PREF = "SharedPref"
        const val KEYBOARD = "keyboard"
        const val RUN_IN_JUSPAY_WEBVIEW = "run_in_juspay_webview"
        const val JBRIDGE = "jbridge"
        const val UTIL = "util"
        const val UPI_UTILS = "upi_utils"
        const val PLAY_AUDIO = "play_audio"
        const val CL_ELIGIBILITY = "cl_eligibility"
        const val MPIN_UTIL = "mpin_util"
        const val LOG_PUSHER = "log_pusher"
        const val PAYMENT_SESSION_INFO = "payment_session_info"
        const val SESSION_INFO = "session_info"
        const val FILE_PROVIDER_SERVICE = "file_provider_service"
        const val REMOTE_ASSET_SERVICE = "remote_asset_service"
        const val JUSPAY_WEBVIEW_CLIENT = "juspay_webview_client"
        const val IS_NETWORK_AVAILABLE = "is_network_available"
        const val READ_SMS_PERMISSION = "read_sms_permission"
        const val PERMISSION = "permission"
        const val HELPER = "helper"
        const val GODEL_SERVICE_CONNECTION = "godel_service_connection"
        const val GODEL_SERVICE_RESPONSE_HANDLER = "godel_service_response_handler"
        const val GODEL_WEBVIEW_WHITELIST = "godel_webview_whitelist"
    }

    object User {
        const val SHOULD_DISABLE_GODEL = "should_disable_godel"
    }

    object Network {
        const val BEFORE_REQUEST = "before_request"
        const val NETWORK_CALL = "network_call"
        const val CANCEL_API = "cancel_api"
    }

    object Device {
        const val MEMORY = "memory"
        const val IDENTIFIERS = "identifiers"
        const val SESSION_INFO = "session_info"
        const val PHONE_STATE = "phone_state"
    }
}
