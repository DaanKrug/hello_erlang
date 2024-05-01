
CREATE TABLE  `wallet` (
  `id` bigint(20) UNSIGNED NOT NULL,
  `identifier` varchar(100) COLLATE utf8mb4_general_ci NOT NULL,
  `password` text COLLATE utf8mb4_general_ci NOT NULL,
  `recoverinfo` text COLLATE utf8mb4_general_ci NOT NULL,
  `active` tinyint(1) NOT NULL DEFAULT 1,
  `loginFails` int(1) UNSIGNED DEFAULT 0,
  `totalRewards` bigint(20) UNSIGNED DEFAULT 0,
  `totalCoins` decimal(21,6) UNSIGNED DEFAULT 0,
  `ownerId` bigint(20) UNSIGNED DEFAULT NULL,
  `created_at` timestamp NULL DEFAULT NULL,
  `updated_at` timestamp NULL DEFAULT NULL,
  `deleted_at` timestamp NULL DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

ALTER TABLE `wallet`
  ADD PRIMARY KEY (`id`);

ALTER TABLE `wallet`
  MODIFY `id` bigint(20) UNSIGNED NOT NULL AUTO_INCREMENT;
  
  
  
  
CREATE TABLE  `wallettransaction` (
  `id` bigint(20) UNSIGNED NOT NULL,
  `identifierSender` varchar(100) COLLATE utf8mb4_general_ci NOT NULL,
  `identifierReceiver` varchar(100) COLLATE utf8mb4_general_ci NOT NULL,
  `description` varchar(100) COLLATE utf8mb4_general_ci NOT NULL DEFAULT 'Generic Transfer',
  `amountCoins` decimal(21,6) UNSIGNED DEFAULT 0,
  `status` varchar(30) COLLATE utf8mb4_general_ci NOT NULL DEFAULT 'pending',
  `ownerId` bigint(20) UNSIGNED DEFAULT NULL,
  `created_at` timestamp NULL DEFAULT NULL,
  `updated_at` timestamp NULL DEFAULT NULL,
  `deleted_at` timestamp NULL DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

ALTER TABLE `wallettransaction`
  ADD PRIMARY KEY (`id`);

ALTER TABLE `wallettransaction`
  MODIFY `id` bigint(20) UNSIGNED NOT NULL AUTO_INCREMENT;
  
  
  
  
CREATE TABLE `masternode` (
     `id` bigint(20) UNSIGNED NOT NULL,
     `a1_name` varchar(100) COLLATE utf8mb4_general_ci NOT NULL,
     `a2_ip` varchar(30) COLLATE utf8mb4_general_ci NOT NULL,
     `a3_region` varchar(50) COLLATE utf8mb4_general_ci NOT NULL,
     `a4_master` tinyint(1) NOT NULL DEFAULT 0,
     `a5_walletid` bigint(20) UNSIGNED NOT NULL DEFAULT 0,
     `a6_walletidentifier` varchar(100) COLLATE utf8mb4_general_ci NOT NULL,
     `active` tinyint(1) NOT NULL DEFAULT 0,
     `ownerId` bigint(20) UNSIGNED NOT NULL,
     `created_at` timestamp NULL DEFAULT NULL,
     `updated_at` timestamp NULL DEFAULT NULL,
     `deleted_at` timestamp NULL DEFAULT NULL
   ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;
   
ALTER TABLE `masternode` ADD PRIMARY KEY (`id`);
   
ALTER TABLE `masternode` MODIFY `id` bigint(20) UNSIGNED NOT NULL AUTO_INCREMENT; 
  
  
 
  
CREATE TABLE  `block` (
  `id` bigint(20) UNSIGNED NOT NULL,
  `indexx` varchar(50) COLLATE utf8mb4_general_ci NOT NULL,
  `previous` text COLLATE utf8mb4_general_ci NOT NULL,
  `time` varchar(50) COLLATE utf8mb4_general_ci NOT NULL,
  `keyy` varchar(100) COLLATE utf8mb4_general_ci NOT NULL,
  `hash` text COLLATE utf8mb4_general_ci NOT NULL,
  `transactions` int(2) UNSIGNED NOT NULL DEFAULT 0,
  `initiated` int(1) UNSIGNED NOT NULL DEFAULT 0,
  `locked` int(1) UNSIGNED NOT NULL DEFAULT 0,
  `mined_hash` text COLLATE utf8mb4_general_ci,
  `mined_nonse` varchar(50) COLLATE utf8mb4_general_ci,
  `mined_key` text COLLATE utf8mb4_general_ci,
  `miner_walletid` varchar(100) COLLATE utf8mb4_general_ci,
  `public_key` text COLLATE utf8mb4_general_ci,
  `hash_signature` text COLLATE utf8mb4_general_ci,
  `created_at` timestamp NULL DEFAULT NULL,
  `updated_at` timestamp NULL DEFAULT NULL,
  `deleted_at` timestamp NULL DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

ALTER TABLE `block`
  ADD PRIMARY KEY (`id`);

ALTER TABLE `block`
  MODIFY `id` bigint(20) UNSIGNED NOT NULL AUTO_INCREMENT; 
  
  
  

CREATE TABLE  `transaction` (
  `id` bigint(20) UNSIGNED NOT NULL,
  `sender` varchar(100) COLLATE utf8mb4_general_ci NOT NULL,
  `receiver` varchar(100) COLLATE utf8mb4_general_ci NOT NULL,
  `amount` decimal(21,6) UNSIGNED DEFAULT 0,
  `time` varchar(50) COLLATE utf8mb4_general_ci NOT NULL,
  `keyy` varchar(100) COLLATE utf8mb4_general_ci NOT NULL,
  `hash` text COLLATE utf8mb4_general_ci NOT NULL,
  `wallettransactionid` bigint(20) UNSIGNED NOT NULL,
  `blockid` bigint(20) UNSIGNED NOT NULL,
  `miner_walletid` varchar(100) COLLATE utf8mb4_general_ci,
  `public_key` text COLLATE utf8mb4_general_ci,
  `hash_signature` text COLLATE utf8mb4_general_ci,
  `created_at` timestamp NULL DEFAULT NULL,
  `updated_at` timestamp NULL DEFAULT NULL,
  `deleted_at` timestamp NULL DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

ALTER TABLE `transaction`
  ADD PRIMARY KEY (`id`);

ALTER TABLE `transaction`
  MODIFY `id` bigint(20) UNSIGNED NOT NULL AUTO_INCREMENT; 
  