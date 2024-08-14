# DO NOT DELETE/EDIT THIS LINE OF CODE, AS IT IS USED TO PROVIDE ACCESS TO
# THE FUNCTIONS FROM THE PREVIOUS QUESTION
from hidden import comp10001huxxy_valid_table

def comp10001huxxy_valid_play(play, play_history, active_player, hand, table):
    ''' Takes `play`, a 3-tuple representing an attempted play, `play_history, 
    a list of 3-tuples representing all plays that have taken place so far in
    chronological order, `active_player`, an integer between 0 and 3 inclusive
    which represents the player number of the player whose turn it is to play,
    `hand` a list of the cards held by the player attempting the play, and 
    `table`, a list of list of cards representing the table. Returns a Boolean
    indicating whether the play is valid or not given the current game 
    state. '''
    
        
    # break up the `play` tuple to use in testing 
    player_turn, play_type, play_details = play 
    
    # case where supposed player whose turn it is doesn't match active player
    if player_turn != active_player:
        return False
    
    # case where player is picking up a card from stock
    if play_type == 0:
        
        # check if the player's hand is empty:
        if not hand:
            return False
        else:
            return True 

        
    # case where player is playing a card from their hand to the table
    if play_type == 1:
        card, to_group = play_details
        
        # check if the card trying to be played is in the player's hand
        if card not in hand:
            return False
        
        # check if the group number is valid
        if len(table) < to_group:
            return False
        
        # check that no more than 6 plays will be made in this turn
        play_counter = 0
        for past_play in play_history:
            past_player_turn, past_play_type, past_play_details = past_play
            if (past_play_type == 3 or 
                len(play_history) == play_history.index(past_play) - 1):
                if play_counter > 5:
                    return False
                else:
                    play_counter = 0
                    continue
            elif past_play_type == 1 or past_play_type == 2:
                play_counter += 1
        
        # the play must be valid as all exceptions have been passed
        return True
    
    
    # case where the player is moving a card from one group to another
    if play_type == 2:
        card, from_group, to_group = play_details
        
        # check if the card trying to be played is in the player's hand
        # check if the originating group exists
        # check if the card being moved is in the originating group
        # check if the group number is valid
        
        if card not in hand:
            return False
        
        elif len(table) < from_group + 1:
            return False
        
        elif card not in table[from_group]:
            return False
        
        elif len(table) < to_group:
            return False
        
        # check that a turn involving card movement between groups begins with 
        # a play from a player's hand and that no more than 6 plays will be 
        # made in this turn
        turn_type_list = []
        play_counter = 0
        for past_play in play_history:
            past_player_turn, past_play_type, past_play_details = past_play
            if (past_play_type == 3 or 
                len(play_history) == play_history.index(past_play) - 1):
                if turn_type_list[0] != 1 and 2 in turn_type_list:
                    return False
                elif play_counter > 5:
                    return False
                else:
                    turn_type_list = []
                    play_counter = 0
            elif past_play_type == 1 or past_play_type == 2:
                turn_type_list.append(past_play_type)
                play_counter += 1
        
        # the play must be valid as all exceptions have been passed
        return True
                
        
    # case where player is ending their turn after making play(s) on the table    
    if play_type == 3:
        
        # check if player has made a play to the table
        if not play_history:
            return False
        
        # track the score of a turn, whether an opening turn has already 
        # occured, and assign the value of each card to a score for score 
        # calculation
        score = 0
        opening_turn = False
        value_score_dict = {'A': 1, '2': 2, '3': 3, '4': 4, '5': 5, '6': 6, 
                            '7': 7, '8': 8, '9': 9, '0': 10, 'J': 11, 'Q': 12, 
                            'K': 13}
        
        
        # define a turn as all plays made by a player up until a `play_type` of 
        # 3 or a play of `play_type` 0 and find the cards played in these turns
        turn_cards = []
        for past_play in play_history:
            past_player_turn, past_play_type, past_play_details = past_play
            
            # check if a previously completed turn was an opening turn by 
            # combining the score of each card in a turn
            if past_player_turn == active_player:
                if past_play_type == 3:
                    for card in turn_cards:
                        score += value_score_dict[card[0]]
                        if score >= 24:
                            opening_turn = True
                            break
                        turn_cards = []
                elif past_play_type == 0:
                    continue
                elif past_play_type != 3:
                    turn_cards.append(past_play_details[0])
                
           
        # if an opening turn still hasn't been made and the current turn isn't
        # an opening turn then the play has to be invalid
        if not opening_turn:
            for card in turn_cards:
                score += value_score_dict[card[0]]
            if score < 24:
                return False
                  
                   
        return comp10001huxxy_valid_table(table)                                             