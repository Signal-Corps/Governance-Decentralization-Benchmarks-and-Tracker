/* 
Output results available via API at Flipside Crypto: 
https://app.flipsidecrypto.com/velocity/queries/96f1f98a-3ddf-4703-8405-2dbada96a550
*/

SELECT 
  CASE
  	WHEN contract_address = LOWER('0x5e4be8Bc9637f0EAA1A755019e06A68ce081D58F') THEN CONCAT('0.', event_inputs[ 'proposalId' ]::VARCHAR)
    WHEN contract_address = LOWER('0xC4e172459f1E7939D522503B81AFAaC1014CE6F6') THEN CONCAT('1.', event_inputs[ 'proposalId' ]::VARCHAR)
    WHEN contract_address = LOWER('0x408ED6354d4973f66138C91495F2f2FCbd8724C3') THEN CONCAT('2.', LPAD(event_inputs[ 'proposalId' ], 2,0)::VARCHAR)
  END as proposal,
  /*
  CASE
  	WHEN contract_address = LOWER('0x5e4be8Bc9637f0EAA1A755019e06A68ce081D58F') THEN event_inputs[ 'proposalId' ]::VARCHAR
  	WHEN contract_address = LOWER('0xC4e172459f1E7939D522503B81AFAaC1014CE6F6') THEN event_inputs[ 'proposalId' ]::VARCHAR
  	WHEN contract_address = LOWER('0x408ED6354d4973f66138C91495F2f2FCbd8724C3') THEN LPAD(event_inputs[ 'proposalId' ], 2,0)::VARCHAR
  END as proposal_id,
*/
  --CONCAT(version, proposal_id) as proposal, --proposal naming convention as per https://app.uniswap.org/#/vote?chain=mainnet with exception of correction for 2.9 to 2.09
  CASE 
    WHEN event_inputs[ 'support' ] = true OR event_inputs[ 'support' ]::VARCHAR = '1' then 'For' 
    WHEN event_inputs[ 'support' ] = false OR event_inputs[ 'support' ]::VARCHAR = '0' then 'Against'
  	ELSE event_inputs[ 'support' ]
  END as support, 
    event_inputs[ 'voter' ] as voter_address, 
    event_inputs[ 'votes' ] / pow(10, 18) as votes, 
  block_number,
  block_timestamp,
  contract_address,
  contract_name,
  tx_hash,
  event_index
  FROM 
    ethereum.core.fact_event_logs
  where 
    contract_address IN (LOWER('0x5e4be8Bc9637f0EAA1A755019e06A68ce081D58F') --GovernorAlpha
  						, LOWER('0xC4e172459f1E7939D522503B81AFAaC1014CE6F6') --GovernorAlpha v2
  						, LOWER('0x408ED6354d4973f66138C91495F2f2FCbd8724C3') --GovernorBravo
  )
    and event_name ='VoteCast'
    and block_timestamp >=  '2020-09-01'
  	and proposal != '0.5 VOID' --filter out DeFi Education Fund proposal on GovernorAlpha 
    AND tx_status = 'SUCCESS'
  ;
