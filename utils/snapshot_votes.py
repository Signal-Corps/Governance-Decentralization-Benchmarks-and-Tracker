import pandas as pd
import numpy as np
import math
import requests
import time
import sys

def run_query(query): # A simple function to use requests.post to make the API call. Note the json= section.
    """
    Use requests.post to make API call to Snapshot GraphQL API
    Snapshot API Documentation: https://docs.snapshot.org/graphql-api

    Parameters
    ----------
    query: str
        GraphQL query

    Returns
    -------
    request: dictionary
        JSON response payload

    """
    request = requests.post('https://hub.snapshot.org/graphql', json={'query': query})
    if request.status_code == 200:
        return request.json()
    else:
        raise Exception("Query failed to run by returning code of {}. {}".format(request.status_code, query))

def get_proposals(space_id):
    """
    Query closed proposals within a space on Snapshot

    Parameters
    ----------
    space_id : str
        Unique identifier for project in Snapshot

    Returns
    -------
    proposals_df : pandas dataframe
        Dataframe of closed proposals

    """

    query = (
    """
    query Proposals {
          proposals (
            first: 100000,
            skip: 0,
            where: {
    """
    +
            '\n space_in :["{}"], \n'.format(space_id)
    +
    """
         state: "closed"
            },
            orderBy: "created",
            orderDirection: desc
          ) {
            id
            title
            body
            choices
            start
            end
            snapshot
            state
            author
            space {
              id
              name
            }
          }
        }
    """
    )
    proposals = run_query(query) # Execute the query
    proposals_df = pd.json_normalize(proposals['data']['proposals'])
    return proposals_df


def get_votes(space_id, proposal_id):
    """
    Query votes for a proposal

    Parameters
    ----------
    space_id : str
        Unique identifier for project in Snapshot
    proposal_id : str
        Specific proposal for which votes are being requested

    Returns
    -------
    votes_df : pandas dataframe
        Dataframe consisting of votes per address with choice selected and voting power

    """
    query = (
    """
    {
      votes (
        first: 1000000
        where: {
    """
    +
            '\n space_in :["{}"], \n'.format(space_id)
    +
            '\n proposal :"{}" \n'.format(proposal_id)
    +
    """
            }
      ) {
        proposal {
          id
          title
          choices
        }
        voter
        vp
        vp_state
        created
        choice
        space {
          id
        }
      }
    }
    """
    )
    votes = run_query(query)
    votes_df = pd.json_normalize(votes['data']['votes'])
    return votes_df


def choice_string(choice, proposal_choices):
    """
    Return string value for choice and perform some transformation in case of value and type errors

    Parameters
    ----------
    choice : int, list
        Integer or list of ranked choices
    proposal_choices : list
        List of possible choices for proposal

    Returns
    -------
    choice_str : str
        String value for choice selected by voter

    """
    if choice == []:
        return None
    if isinstance(choice, list):
        choice = choice[0]
    if "[" in str(choice):
        choice = int(choice.strip('][').split(',')[0])
    if math.isnan(float(choice)) == True:
        return None
    choice_str = proposal_choices[int(float(choice) -1)]
    return choice_str

def get_votes_dataframe(space_id, proposals):
    """
    Query votes for Snapshot proposals
    The Snapshot API has a limit of 30K rows per request so votes are requested on a proposal
    and then concatenated to bypass limitation

    Parameters
    ----------
    space_id : str
        Unique identifier for project in Snapshot
    proposals : list
        List of proposal ids

    Returns
    -------
    votes_df : pandas dataframe
        Dataframe consisting of concatenated responses of votes for all proposals requested

    """
    votes_dict = {proposal: get_votes(space_id, proposal) for proposal in proposals}
    votes_df = pd.concat(votes_dict.values(), ignore_index=True)
    votes_df['created'] = pd.to_datetime(votes_df['created'], unit= 's', origin='unix')
    votes_df['choice_str'] = votes_df.apply(lambda x : choice_string(x['choice'], x['proposal.choices']), axis=1)
    votes_df = votes_df.rename(columns = {'vp' : 'voting_power', 'choice' : 'choice_int', 'created' : 'created_date'})
    return votes_df



if __name__ == '__main__':
    spaces = ['uniswap', 'sushigov.eth', 'curve.eth', 'cake.eth','balancer.eth']
    proposals_dict = {}
    votes_dict = {}
    for space_id in spaces:
        print(f'Pulling {space_id} data...')
        proposals_dict[space_id] = get_proposals(space_id)
        votes_dict[space_id] = get_votes_dataframe(space_id, proposals_dict[space_id].id.values.tolist())
        time.sleep(60) # artifical latency to prevent to many requests being sent consecutively and causing 504 Gateway Timeout Error
    snapshot_proposals_df = pd.concat(proposals_dict.values(), ignore_index=True)
    snapshot_proposals_df.to_csv(f'snapshot_proposals_df_{time.strftime("%Y%m%d-%H%M%S")}.csv', index=False)
    snapshot_votes_df = pd.concat(votes_dict.values(), ignore_index=True)
    snapshot_votes_df.to_csv(f'snapshot_votes_df_{time.strftime("%Y%m%d-%H%M%S")}.csv', index=False)
