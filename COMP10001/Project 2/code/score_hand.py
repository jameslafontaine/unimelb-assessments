def comp10001huxxy_score(cards):
    ''' Takes a list of cards `cards` held by a player, each in the format of a 
    2-letter string. Returns a positive integer indicating the score for the 
    combined cards based on the face value of each card. '''
    
    # tracks the combined score based on the value of each card
    score = 0
    
    # dictionary that assigns each card value to a score
    value_score_dict = {'A': 1, '2': 2, '3': 3, '4': 4, '5': 5, '6': 6, '7': 7, 
                        '8': 8, '9': 9, '0': 10, 'J': 11, 'Q': 12, 'K': 13}
    
    # sum up the total score by using the assigned score of each card value
    for card in cards:
        score += value_score_dict[card[0]]
            
    return score
                            
    