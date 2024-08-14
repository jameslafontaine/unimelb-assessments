from itertools import combinations

# tracks if the player has already opened or not and game state information 
# allowing for formulation of a full turn at the start of the turn, and then 
# simply for the execution of the planned turn without requiring further
# calculation
game_state_dict = {'current_turn': [], 'has_opened': False, 'play_counter': 0, 
                   'new_group_index': 0}



def comp10001huxxy_play(play_history, active_player, hand, table):
    ''' Takes `play_history, a list of 3-tuples representing all plays that 
    have taken place so far in chronological order, `active_player`, an integer 
    between 0 and 3 inclusive which represents the player number of the player 
    whose turn it is to play, `hand` a list of the cards held by the player 
    attempting the play, and `table`, a list of list of cards representing the 
    table. Returns `play`, a 3-tuple representing the calculated play based on
    the strategy of making as high-valued as possible new groups. `play`
    contains `player_turn`, an integer between 0-3 representing whose turn it 
    is, `play_type`, an integer between 0-3 representing the type of play, and 
    `play_details`, the card used in the play and where it is being moved '''
    
   
    
    # create test hand and table to not mutate them in the game
    test_table = table.copy()
    test_hand = hand.copy()
    
    # assign all the game state data to variables used to make the next play
    # if applicable
    play_counter = game_state_dict['play_counter']
    in_progress_turn = game_state_dict['current_turn']
    new_group = game_state_dict['new_group_index']
        
    # check if player is in the middle of a turn or must end their turn
    if game_state_dict['play_counter'] > 0:
        
        # if the player has made all their plays in the planned turn, end the 
        # turn and reset the game state data for next turn
        if game_state_dict['play_counter'] == len(in_progress_turn):
            game_state_dict['play_counter'] = 0
            game_state_dict['current_turn'] = []
            game_state_dict['new_group_index'] = 0
            return (active_player, 3, None)
        
        else:
            game_state_dict['play_counter'] += 1
            return (active_player, 1, (in_progress_turn[play_counter], 
                                       new_group))
    
    
    # check if player is in the middle of a turn without being in the game from 
    # the beginning for the example function call
    example_play_counter = 0
    for past_play in play_history[-1:-7:-1]:
        past_player_turn, past_play_type, past_play_details = past_play
        if past_player_turn == active_player:
            example_play_counter += 1
            if example_play_counter == 5:
                return (active_player, 3, None)
           
    
         
    # will be used to create and track the score of formulated turns to 
    # ideally find the highest score play which is valid
    highest_score_turn = []
    highest_score = 0
    value_score_dict = {'A': 1, '2': 2, '3': 3, '4': 4, '5': 5, '6': 6, 
                        '7': 7, '8': 8, '9': 9, '0': 10, 'J': 11, 'Q': 12, 
                        'K': 13}
    
    
    # check as many potential turns involving up to 5 plays as possible without 
    # exceeding the allowed time
    for i in range(1, 6):
        
        # all combinations of cards in the hand which could make up a turn 
        hand_combinations = list(combinations(test_hand, i))
                    
        # track the score of a potential turn to find the highest potential
        # turn
        for potential_turn in hand_combinations:
            score = 0
            
            # check if a potential turn is valid
            if comp10001huxxy_valid_table([list(potential_turn)]):
                
                # thus find out the score value of the potential turn
                for card in potential_turn:
                    score += value_score_dict[card[0]]
                                     
                # first check if player still has to make an opening 
                # turn, simply make it if one is needed and one has been found, 
                # otherwise check a new potential turn
                if not game_state_dict['has_opened']:
                    if score >= 24:
                        game_state_dict['has_opened'] = True   
                        game_state_dict['current_turn'] = potential_turn
                        game_state_dict['play_counter'] = 1
                        game_state_dict['new_group_index'] = len(test_table)
                        return (active_player, 1, (potential_turn[0], 
                                                   len(test_table)))
                    else:
                        continue
                            
                # if the score of the current turn is higher than all
                # previous turns then record it
                elif score > highest_score:       
                    highest_score_turn = potential_turn
        
        
    # take the highest value turn found to be valid and make the first play,
    # if no play was found then pick up a card
    if highest_score_turn:
        game_state_dict['current_turn'] = highest_score_turn
        game_state_dict['play_counter'] = 1
        game_state_dict['new_group_index'] = len(test_table)
        return (active_player, 1, (highest_score_turn[0], len(test_table)))
    else:
        return (active_player, 0, None)
      
        
   
                                    


def comp10001huxxy_valid_table(groups):
    ''' Takes `groups` a list of lists of cards (each a 2-element string where 
    the first letter is the card value and the second letter is the card suit) 
    where each list of cards represents a single group on the table. Evaluates 
    whether the table state is valid or not by checking if each group is of the
    'N-of-a-kind form' or 'run' form and returns a bool denoting this 
    evaluation. '''

    # copy of groups for use in this function
    valid_groups = groups.copy()
    
    # address the case in which the table is empty
    if not valid_groups:
        return True
    
    
    # tracks the number of each suit in an 'N-of-a-kind' group
    suit_dict = {'C': 0, 'S': 0, 'H': 0, 'D': 0}
    
    # used to convert card values into values which will enable easier sorting
    sort_value_dict = {'A': '01', '2': '02', '3': '03', '4': '04', '5': '05',
                         '6': '06', '7': '07', '8': '08', '9': '09', '0': '10', 
                          'J': '11', 'Q': '12', 'K': '13'}
    
    # used to convert cards back to original form after sorting
    old_value_dict = {'01': 'A', '02': '2', '03': '3', '04': '4', '05': '5',
                         '06': '6', '07': '7', '08': '8', '09': '9', '10': '0', 
                          '11': 'J', '12': 'Q', '13': 'K'}
    
    
    for valid_group in valid_groups:
        
        # make sure cards are in original format
        for i in range(len(valid_group)):
            if len(valid_group[i]) > 2:
                valid_group[i] = (old_value_dict[valid_group[i][0:2]] + 
                                  valid_group[i][2])
            
                
                
        # check if every group has at least 3 cards
        if len(valid_group) < 3:
            return False
        
        
        # check if the group is of the 'N-of-a-kind' form
        
        # track whether a group could still be of the 'N-of-a-kind' form 
        n_of_a_kind = True
        
        # check that all card values in the group are the same and record
        # the number of each suit present in the group
        for valid_card in valid_group:
            suit_dict[valid_card[1]] += 1
            if valid_group[0][0] != valid_card[0]:
                n_of_a_kind = False
                break
        
        # check that there are unique suits in a 3-of-a-kind or all suits 
        # present in a larger group, continue to next group as group is valid
        if len(valid_group) == 3 and n_of_a_kind is True:
            if max(suit_dict.values()) == 1:
                continue
        elif 0 not in suit_dict.values() and n_of_a_kind is True:
            continue
           
                           
        # check if the group is of the 'run' form
        
        # replace cards with cards containing more appropriate values to make 
        # the list easier to sort
        for i in range(len(valid_group)):
            valid_group[i] = (sort_value_dict[valid_group[i][0]] + 
                              valid_group[i][1])
        
        for i in range(len(valid_group) - 1): 
            
            # the current and proceeding values and suits of a pair of cards
            current_value = int(sorted(valid_group)[i][0:2])
            current_suit = sorted(valid_group)[i][2]
            next_value = int(sorted(valid_group)[i + 1][0:2])
            next_suit = sorted(valid_group)[i + 1][2]
              
                             
            # check that all cards in the group form a continuous sequence in 
            # terms of value, if not then the group must be invalid, convert 
            # cards back to original form
            if current_value != next_value - 1:
                for i in range(len(valid_group)):
                    valid_group[i] = (old_value_dict[valid_group[i][0:2]] + 
                                      valid_group[i][2])
                return False
            
            # check that the sequence is alternating in colour, if not
            # then the group must be invalid as it has failed all tests, 
            # convert cards back to original form
            if current_suit == 'H' or current_suit == 'D':
                if next_suit == 'S' or next_suit == 'C': 
                    continue
                else:
                    for i in range(len(valid_group)):
                        valid_group[i] = (old_value_dict[valid_group[i][0:2]] + 
                                          valid_group[i][2])
                    return False
            elif current_suit == 'S' or current_suit == 'C':
                if next_suit == 'H' or next_suit == 'D':
                    continue
                else:
                    for i in range(len(valid_group)):
                        valid_group[i] = (old_value_dict[valid_group[i][0:2]] + 
                                          valid_group[i][2])
                    return False  
        
        # convert all cards in the group back to their original form
        for i in range(len(valid_group)):
            valid_group[i] = (old_value_dict[valid_group[i][0:2]] + 
                              valid_group[i][2])
            
    # all groups have been tested as valid and so the table state is valid,                      
    return True    