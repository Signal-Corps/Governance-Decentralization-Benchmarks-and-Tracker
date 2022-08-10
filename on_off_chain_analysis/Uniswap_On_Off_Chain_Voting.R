options(scipen = 999)

library(tidyverse)
library(ggplot2)
library(scales)
library(vegan)
library(fmsb)


# Proposal Title Descriptions --------
## List prepared to include Temperature and Consensus Checks with an on-chain vote (excludes canceled on-chain proposals)

df_uni_proposal_descriptions <- as_tibble(read_csv("Uniswap_Governance_Proposal_Descriptions_20220806.csv"))


proposals_temperature <- df_uni_proposal_descriptions %>%
  drop_na(description_snapshot_temperature) %>%
  pull(description_snapshot_temperature)

proposals_consensus <- df_uni_proposal_descriptions %>%
  drop_na(description_snapshot_consensus) %>%
  pull(description_snapshot_consensus) 

proposals_on_chain <- df_uni_proposal_descriptions %>%
  filter(!is.na(description_snapshot_temperature) | !is.na(description_snapshot_consensus)) %>%
  pull(description_on) 

proposals_on_chain_number <- df_uni_proposal_descriptions %>%
  pull(proposal) 


df_uni_proposal_descriptions_long <- df_uni_proposal_descriptions %>%
  rename(temperature = description_snapshot_temperature ,
         consensus = description_snapshot_consensus ,
         on_chain = description_on) %>%
  pivot_longer(cols = c(on_chain, temperature, consensus) ,
               names_to = "stage" ,
               values_to = "proposal.title")


# Data Import - Snapshot ---------

df_snap_uni_select <- as_tibble(read_csv("snapshot_votes_df_20220806-1656.csv")) %>%
  filter(space.id=="uniswap") %>% 
  filter(proposal.title %in% c(proposals_temperature, proposals_consensus)) # Filter Snapshot data to be consistent list of proposals with on-chain vote


# Data Import - On Chain ---------

df_on_uni <- as_tibble(read_csv("uniswap_proposals_df_20220806-2250.csv")) %>%
  rename_all(tolower) %>%
  filter(proposal %in% proposals_on_chain_number)

# Unique Voting Participants and Sum of Voting Power ---------

snap_count <- df_snap_uni_select %>%
  group_by(proposal.title) %>%
  summarise(voting_participants = n_distinct(voter),
            voting_power = sum(voting_power))

on_count <- df_on_uni %>%
  group_by(proposal) %>%
  summarise(voting_participants = n_distinct(voter_address),
            voting_power = sum(votes))

# HHI ---------

snap_hhi <- df_snap_uni_select %>%
  group_by(proposal.title) %>%
  mutate(vote_share = voting_power / sum(voting_power)) %>%
  ungroup() %>%
  mutate(hhi = (vote_share*100)^2) %>%
  group_by(proposal.title) %>%
  summarise(hhi = sum(hhi)) %>%
  ungroup()

on_hhi <- df_on_uni %>%
  group_by(proposal) %>%
  mutate(vote_share = votes / sum(votes)) %>%
  ungroup() %>%
  mutate(hhi = (vote_share*100)^2) %>%
  group_by(proposal) %>%
  summarise(hhi = sum(hhi)) %>%
  ungroup()

# Nakamoto ---------

snap_nakamoto <- df_snap_uni_select %>%
  group_by(proposal.title) %>%
  mutate(vote_share = voting_power / sum(voting_power)) %>%
  ungroup() %>%
  arrange(proposal.title, desc(vote_share)) %>%
  group_by(proposal.title) %>%
  mutate(vote_share_cum = cumsum(vote_share),
         vote_share_cum_lag1 = lag(vote_share_cum),
         vote_share_rank = rank(-vote_share, ties.method = "first") %>% as.numeric,
         nakamoto = if_else(vote_share_rank==1 & vote_share_cum>=0.51, 
                            vote_share_rank ,
                            if_else(vote_share_cum>=0.51 & vote_share_cum_lag1<=0.51,
                                    vote_share_rank, 0))) %>%
  ungroup() %>%
  group_by(proposal.title) %>%
  summarise(nakamoto = sum(nakamoto)) %>%
  ungroup()

on_nakamoto <- df_on_uni %>%
  group_by(proposal) %>%
  mutate(vote_share = votes / sum(votes)) %>%
  ungroup() %>%
  arrange(proposal, desc(vote_share)) %>%
  group_by(proposal) %>%
  mutate(vote_share_cum = cumsum(vote_share),
         vote_share_cum_lag1 = lag(vote_share_cum),
         vote_share_rank = rank(-vote_share, ties.method = "first") %>% as.numeric,
         nakamoto = if_else(vote_share_rank==1 & vote_share_cum>=0.51, 
                            vote_share_rank ,
                            if_else(vote_share_cum>=0.51 & vote_share_cum_lag1<=0.51,
                                    vote_share_rank, 0))) %>%
  ungroup() %>%
  group_by(proposal) %>%
  summarise(nakamoto = sum(nakamoto)) %>%
  ungroup()

# Shannon ---------

snap_shannon <- df_snap_uni_select %>%
  group_by(voter, proposal.title) %>%
  summarise(voting_power = sum(voting_power)) %>%
  ungroup() %>%
  group_by(proposal.title) %>%
  mutate(shannon = diversity(voting_power)) %>%
  summarise(shannon = mean(shannon)) %>%
  ungroup()

on_shannon <- df_on_uni %>%
  group_by(voter_address, proposal) %>%
  summarise(voting_power = sum(votes)) %>%
  ungroup() %>%
  group_by(proposal) %>%
  mutate(shannon = diversity(voting_power)) %>%
  summarise(shannon = mean(shannon)) %>%
  ungroup()



# Gini ---------

snap_gini <- df_snap_uni_select %>%
  group_by(voter, proposal.title) %>%
  summarise(voting_power = sum(voting_power)) %>%
  ungroup() %>%
  group_by(proposal.title) %>%
  mutate(gini = DescTools::Gini(voting_power)) %>%
  summarise(gini = mean(gini)) %>%
  ungroup()


on_gini <- df_on_uni %>%
  group_by(voter_address, proposal) %>%
  summarise(voting_power = sum(votes)) %>%
  ungroup() %>%
  group_by(proposal) %>%
  mutate(gini = DescTools::Gini(voting_power)) %>%
  summarise(gini = mean(gini)) %>%
  ungroup()

# Summary -------------

snap_summary <- list(snap_count, 
                                   snap_nakamoto,
                                   snap_hhi, 
                                   snap_gini,
                                   snap_shannon
) %>%
  reduce(left_join, by = c("proposal.title")) %>%
  mutate(stage = case_when(proposal.title %in% proposals_temperature ~ "temperature" ,
                           proposal.title %in% proposals_consensus ~ "consensus"))


on_summary <- list(on_count, 
                   on_nakamoto,
                   on_hhi, 
                   on_gini,
                   on_shannon
) %>%
  reduce(left_join, by = c("proposal")) %>%
  mutate(stage = "on_chain")


coalesce_join <- function(x, y, 
                          by = NULL, suffix = c(".x", ".y"), 
                          join = dplyr::full_join, ...) {
  joined <- join(x, y, by = by, suffix = suffix, ...)
  # names of desired output
  cols <- union(names(x), names(y))
  
  to_coalesce <- names(joined)[!names(joined) %in% cols]
  suffix_used <- suffix[ifelse(endsWith(to_coalesce, suffix[1]), 1, 2)]
  # remove suffixes and deduplicate
  to_coalesce <- unique(substr(
    to_coalesce, 
    1, 
    nchar(to_coalesce) - nchar(suffix_used)
  ))
  
  coalesced <- purrr::map_dfc(to_coalesce, ~dplyr::coalesce(
    joined[[paste0(.x, suffix[1])]], 
    joined[[paste0(.x, suffix[2])]]
  ))
  names(coalesced) <- to_coalesce
  
  dplyr::bind_cols(joined, coalesced)[cols]
}


coalesce_join(df1, df2, by = 'key')


df_summary <- df_uni_proposal_descriptions_long %>%
  left_join(snap_summary, by = c("proposal.title", "stage")) %>% 
  coalesce_join(on_summary, by = c("proposal", "stage")) 


# Save Objects for Shiny App ------------

saveRDS(df_summary, file = "df_summary.rds") 


# Scatter Plots ------------

ggplot(df_summary, aes(x=voting_participants, y=hhi, color=stage)) + 
  geom_point(size = 2) + 
  labs(color = "Phase")

ggplot(df_summary, aes(x=voting_participants, y=gini, color=stage)) + 
  geom_point(size = 2) + 
  labs(color = "Phase")

ggplot(df_summary, aes(x=shannon, y=hhi, color=stage)) + 
  geom_point(size = 2) + 
  labs(color = "Phase") +
  ggtitle("Figure 1.  Comparison of HHI and Shannon Diversity Index Scores by Phase") +
  theme(plot.title=element_text(size=11) ,
        legend.position = "bottom")

