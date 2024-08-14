def infer_bird_species(environment, observations, region_list):
    # TODO implement this function.
    '''
    
    '''
    
    number_of_factors = len(environment[0][0])
    # stores all regions linked to a species of bird
    species_region_dict = {}
    
    # record all regions in which a species was found along with its 
    # environmental factors
    for lst in range(len(observations)):
        for region in range(len(observations[lst])):
            for bird in range(len(observations[lst][region])):
                if observations[lst][region][bird] not in species_region_dict:
                    species_region_dict[observations[lst][region][bird]] = []
                species_region_dict[observations[lst][region][bird]] += environment[lst][region]
    
    number_of_regions = region + 1
    
    species_region_dict = {'magpie': [1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 0], 'yellow robin': [1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1], 'warbler': [1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1]})

    # identify which environmental factors are present in all of the regions
    # in which a bird species was observed
    number_of_regions = 3
    number_of_factors = 6
    common_factor_dict = {}
    for bird in species_region_dict:
        common_conditions = False
        for factor in range(number_of_factors + 1):
            i = 0
            while i < number_of_regions:
                if species_region_dict[bird][i + factor + number_of_factors] == species_region_dict[bird][i + factor]:
                    common_conditions = True
                    i += number_of_factors
                else:
                    common_conditions = False
        # if bird not in common_factor_dict:
        common_factor_dict[bird]= []
        if common_conditions is True:
            common_factor_dict[bird].append(1)
        else:
            common_factor_dict[bird].append(0)
            
        print(common_factor_dict)

# environment = [[[1, 0, 0, 1, 1, 1], [1, 0, 0, 1, 1, 1], [1, 1, 1, 1, 0, 0], [1, 0, 1, 1, 1, 0], [1, 1, 0, 1, 0, 1]], [[1, 0, 1, 0, 1, 1], [1, 1, 1, 1, 0, 0], [1, 0, 1, 1, 1, 0], [1, 1, 0, 0, 1, 1], [1, 1, 1, 0, 1, 0]], [[0, 1, 1, 0, 1, 1], [1, 1, 1, 1, 0, 0], [1, 0, 1, 0, 1, 1], [1, 0, 0, 1, 1, 1], [1, 0, 0, 1, 1, 1]], [[1, 1, 0, 0, 1, 1], [1, 0, 1, 0, 1, 1], [1, 1, 1, 1, 0, 0], [1, 0, 1, 1, 0, 1], [1, 1, 1, 0, 0, 1]], [[1, 1, 0, 1, 0, 1], [0, 1, 1, 1, 0, 1], [1, 1, 0, 1, 1, 0], [0, 1, 0, 1, 1, 1], [1, 1, 1, 1, 0, 0]]]
# observations = [[['magpie', 'yellow robin', 'warbler'], ['magpie', 'yellow robin', 'warbler'], ['magpie'], ['magpie', 'yellow robin'], ['magpie', 'warbler']], [['yellow robin'], ['magpie'], ['magpie', 'yellow robin'], ['yellow robin'], ['yellow robin']], [[], ['magpie'], ['yellow robin'], ['magpie', 'yellow robin', 'warbler'], ['magpie', 'yellow robin', 'warbler']], [['yellow robin'], [], ['magpie'], ['magpie', 'warbler'], []], [['magpie', 'warbler'], ['warbler'], [], ['warbler'], ['magpie']]]
