------- SQL updates -------


-- NEED to run this in master / prod to delete access matrix queries for earning/summary and payout/list api

-- Remove access for deleted Finance Payout List API
-- {"api":"GetFinanceManagementFinancePayoutList","migration":"removeAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bpp_dashboard"}
DELETE FROM atlas_bpp_dashboard.access_matrix
WHERE role_id = '37947162-3b5d-4ed6-bcac-08841be1534d'
  AND api_entity = 'DSL'
  AND user_action_type = 'PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/GET_FINANCE_MANAGEMENT_FINANCE_PAYOUT_LIST';


-- Remove access for deleted Finance Earning Summary API
-- {"api":"GetFinanceManagementFinanceEarningSummary","migration":"removeAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bpp_dashboard"}
DELETE FROM atlas_bpp_dashboard.access_matrix
WHERE role_id = '37947162-3b5d-4ed6-bcac-08841be1534d'
  AND api_entity = 'DSL'
  AND user_action_type = 'PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/GET_FINANCE_MANAGEMENT_FINANCE_EARNING_SUMMARY';

