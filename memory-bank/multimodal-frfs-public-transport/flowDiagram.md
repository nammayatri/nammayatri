# Multimodal Public Transport (FRFS) Flow: Comprehensive Diagram

This diagram provides a high-level overview of the core interactions within the Multimodal Public Transport (FRFS) flow. For detailed API specifications, data structures, and module responsibilities, please refer to `productContext.md`, `techContext.md`, and `systemPatterns.md`.

```mermaid
graph TD
    subgraph UI Layer
        UI[User Interface]
    end

    subgraph Backend Services
        AuthSvc[Authentication Service]
        FRFSTicketSvc[FRFSTicketService]
        MultimodalConfirmSvc[MultimodalConfirm]
        PaymentSvc[Payment Service]
        RiderLocationSvc[Rider Location Service]
        JourneyPlanningLogic[Journey Planning Logic]
        MultimodalDomainModels[Multimodal Domain Models]
    end

    subgraph External BPP Integration
        ExternalBPPFlow[ExternalBPP.Flow]
        ExternalBPPCallAPI[ExternalBPP.CallAPI]
        BecknACL[Beckn.ACL.FRFS.*]
        OnActionHandlers[Domain.Action.Beckn.FRFS.On*]
        CallFRFSBPP[SharedLogic.CallFRFSBPP]
        CRIS[CRIS Provider (Subway)]
        EBIX[EBIX Provider (Bus)]
        CMRL[CMRL Provider (Metro)]
        OtherDirectBPPs[Other Direct BPPs]
    end

    subgraph Data & External Systems
        DB[Database]
        OTPRestClient[OTPRest Client (Nandi)]
        GTFSData[External GTFS/OTP Service]
        PaymentGateway[External Payment Gateway]
        MappingService[External Mapping Service (OSRM)]
    end

    UI -- Authenticate --> AuthSvc
    AuthSvc -- Auth Result --> UI

    UI -- Search Request --> FRFSTicketSvc
    FRFSTicketSvc -- Get Configs --> DB
    DB -- Configs --> FRFSTicketSvc
    FRFSTicketSvc -- Create Search Record --> DB
    FRFSTicketSvc -- Delegate Search --> ExternalBPPCallAPI

    ExternalBPPCallAPI -- Build Beckn Req --> CallFRFSBPP
    CallFRFSBPP -- Send Req --> BecknGateway[Beckn Gateway]
    BecknGateway -- Forward Req --> BPPs[External BPPs (Beckn)]

    ExternalBPPFlow -- Concurrent Search --> OTPRestClient
    OTPRestClient -- Fetch Data --> GTFSData
    OTPRestClient -- Data Fallback --> DB

    ExternalBPPCallAPI -- Call CRIS --> CRIS
    ExternalBPPCallAPI -- Call EBIX --> EBIX
    ExternalBPPCallAPI -- Call CMRL --> CMRL
    ExternalBPPCallAPI -- Call Other Direct --> OtherDirectBPPs

    BPPs -- on_search Callback --> BecknGateway
    BecknGateway -- Forward Callback --> OnActionHandlers
    OnActionHandlers -- Process, Persist Quote --> DB

    FRFSTicketSvc -- Get Quotes --> DB
    DB -- Quotes --> FRFSTicketSvc
    FRFSTicketSvc -- Return Quotes --> UI

    UI -- Select Quote, Initiate Journey --> FRFSTicketSvc / MultimodalConfirmSvc
    FRFSTicketSvc / MultimodalConfirmSvc -- Create Booking Record --> DB
    FRFSTicketSvc / MultimodalConfirmSvc -- Delegate Init --> ExternalBPPCallAPI

    ExternalBPPCallAPI -- Build Beckn Init Req --> CallFRFSBPP
    CallFRFSBPP -- Send Init Req --> BecknGateway
    BecknGateway -- Forward Init Req --> BPPs

    BPPs -- on_init Callback --> BecknGateway
    BecknGateway -- Forward on_init --> OnActionHandlers
    OnActionHandlers -- Process, Update Booking Status --> DB

    UI -- Confirm Booking --> FRFSTicketSvc / MultimodalConfirmSvc
    FRFSTicketSvc / MultimodalConfirmSvc -- Initiate Payment --> PaymentSvc
    PaymentSvc -- Interact --> PaymentGateway
    PaymentGateway -- Payment Status --> PaymentSvc
    FRFSTicketSvc / MultimodalConfirmSvc -- Delegate Confirm --> ExternalBPPCallAPI

    ExternalBPPCallAPI -- Build Beckn Confirm Req --> CallFRFSBPP
    CallFRFSBPP -- Send Confirm Req --> BecknGateway
    BecknGateway -- Forward Confirm Req --> BPPs

    BPPs -- on_confirm Callback --> BecknGateway
    BecknGateway -- Forward on_confirm --> OnActionHandlers
    OnActionHandlers -- Process, Update Booking, Create Ticket --> DB

    UI -- View Journey Status / Location Update --> FRFSTicketSvc / RiderLocationSvc
    FRFSTicketSvc / RiderLocationSvc -- Delegate Status --> ExternalBPPCallAPI

    ExternalBPPCallAPI -- Build Beckn Status Req --> CallFRFSBPP
    CallFRFSBPP -- Send Status Req --> BecknGateway
    BecknGateway -- Forward Status Req --> BPPs

    BPPs -- on_status Callback --> BecknGateway
    BecknGateway -- Forward on_status --> OnActionHandlers
    OnActionHandlers -- Process, Update Booking/Ticket Status --> DB

    UI -- Modify/Cancel Journey --> FRFSTicketSvc / MultimodalConfirmSvc
    FRFSTicketSvc / MultimodalConfirmSvc -- Delegate Cancel/Update --> ExternalBPPCallAPI

    ExternalBPPCallAPI -- Build Beckn Cancel/Update Req --> CallFRFSBPP
    CallFRFSBPP -- Send Cancel/Update Req --> BecknGateway
    BecknGateway -- Forward Cancel/Update Req --> BPPs

    BPPs -- on_cancel/on_update Callback --> BecknGateway
    BecknGateway -- Forward on_cancel/on_update --> OnActionHandlers
    OnActionHandlers -- Process, Update Booking Status --> DB

    FRFSTicketSvc -- Distance Calculation --> MappingService

    JourneyPlanningLogic -- Route/Stop Data --> OTPRestClient
    JourneyPlanningLogic -- Distance Calculation --> MappingService
    FRFSTicketSvc -- Calls Journey Planning --> JourneyPlanningLogic
    FRFSTicketSvc -- Interacts with --> MultimodalDomainModels
    ExternalBPPFlow -- Interacts with --> MultimodalDomainModels
    OnActionHandlers -- Updates --> MultimodalDomainModels
    MultimodalDomainModels -- Persisted in --> DB
```
