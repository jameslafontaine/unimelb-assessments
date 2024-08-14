-- __/\\\\\\\\\\\__/\\\\\_____/\\\__/\\\\\\\\\\\\\\\_____/\\\\\_________/\\\\\\\\\_________/\\\\\\\________/\\\\\\\________/\\\\\\\________/\\\\\\\\\\________________/\\\\\\\\\_______/\\\\\\\\\_____        
--  _\/////\\\///__\/\\\\\\___\/\\\_\/\\\///////////____/\\\///\\\_____/\\\///////\\\_____/\\\/////\\\____/\\\/////\\\____/\\\/////\\\____/\\\///////\\\_____________/\\\\\\\\\\\\\___/\\\///////\\\___       
--   _____\/\\\_____\/\\\/\\\__\/\\\_\/\\\_____________/\\\/__\///\\\__\///______\//\\\___/\\\____\//\\\__/\\\____\//\\\__/\\\____\//\\\__\///______/\\\_____________/\\\/////////\\\_\///______\//\\\__      
--    _____\/\\\_____\/\\\//\\\_\/\\\_\/\\\\\\\\\\\____/\\\______\//\\\___________/\\\/___\/\\\_____\/\\\_\/\\\_____\/\\\_\/\\\_____\/\\\_________/\\\//_____________\/\\\_______\/\\\___________/\\\/___     
--     _____\/\\\_____\/\\\\//\\\\/\\\_\/\\\///////____\/\\\_______\/\\\________/\\\//_____\/\\\_____\/\\\_\/\\\_____\/\\\_\/\\\_____\/\\\________\////\\\____________\/\\\\\\\\\\\\\\\________/\\\//_____    
--      _____\/\\\_____\/\\\_\//\\\/\\\_\/\\\___________\//\\\______/\\\______/\\\//________\/\\\_____\/\\\_\/\\\_____\/\\\_\/\\\_____\/\\\___________\//\\\___________\/\\\/////////\\\_____/\\\//________   
--       _____\/\\\_____\/\\\__\//\\\\\\_\/\\\____________\///\\\__/\\\______/\\\/___________\//\\\____/\\\__\//\\\____/\\\__\//\\\____/\\\___/\\\______/\\\____________\/\\\_______\/\\\___/\\\/___________  
--        __/\\\\\\\\\\\_\/\\\___\//\\\\\_\/\\\______________\///\\\\\/______/\\\\\\\\\\\\\\\__\///\\\\\\\/____\///\\\\\\\/____\///\\\\\\\/___\///\\\\\\\\\/_____________\/\\\_______\/\\\__/\\\\\\\\\\\\\\\_ 
--         _\///////////__\///_____\/////__\///_________________\/////_______\///////////////_____\///////________\///////________\///////_______\/////////_______________\///________\///__\///////////////__

-- Your Name: James La Fontaine
-- Your Student Number: 1079860
-- By submitting, you declare that this work was completed entirely by yourself.

-- ____________________________________________________________________________________________________________________________________________________________________________________________________________
-- BEGIN Q1

SELECT DISTINCT first_name, last_name 
FROM staff 
INNER JOIN profile ON staff.id = profile.staff_id
INNER JOIN role ON profile.role_id = role.id
WHERE role_name LIKE 'team Lead';   

-- END Q1
-- ____________________________________________________________________________________________________________________________________________________________________________________________________________
-- BEGIN Q2

SELECT DISTINCT last_name, first_name, team_name
FROM staff
INNER JOIN profile ON staff.id = profile.staff_id
INNER JOIN team ON profile.team_id = team.id
WHERE parent_id = (SELECT id
				   FROM team 
                   WHERE team_name LIKE 'Victoria')
ORDER BY last_name;

-- END Q2
-- ____________________________________________________________________________________________________________________________________________________________________________________________________________
-- BEGIN Q3

SELECT DISTINCT first_name, last_name
FROM staff
INNER JOIN profile ON staff.id = profile.staff_id
INNER JOIN team ON profile.team_id = team.id
WHERE team_name LIKE 'Errinundra' AND valid_from <= '20210513' AND valid_until >= '20210413';

-- END Q3
-- ____________________________________________________________________________________________________________________________________________________________________________________________________________
-- BEGIN Q4

SELECT first_name, last_name
FROM staff 
INNER JOIN profile ON staff.id = profile.staff_id 
INNER JOIN role ON profile.role_id = role.id
INNER JOIN team on profile.team_id = team.id
WHERE role_name LIKE 'Agent' AND team_name LIKE 'Werrikimbe' AND valid_from <= 20210523 AND valid_until >= 20210523 
AND last_name NOT IN (SELECT last_name
					  FROM staff 
					  INNER JOIN profile ON staff.id = profile.staff_id
					  INNER JOIN role ON profile.role_id = role.id
					  INNER JOIN team on profile.team_id = team.id 
					  WHERE role_name LIKE 'Agent' AND team_name NOT LIKE 'Werrikimbe' AND valid_from <= 20210523 AND valid_until >= 20210523);

-- END Q4
-- ____________________________________________________________________________________________________________________________________________________________________________________________________________
-- BEGIN Q5

SELECT team_name, MONTH(response_time) AS month, AVG(agent_quality) AS averageAQ
FROM profile
INNER JOIN survey_response AS sr ON profile.profile_ref LIKE sr.profile_ref
INNER JOIN team ON profile.team_id = team.id
GROUP BY team_name, month
ORDER BY averageAQ DESC;

-- END Q5
-- ____________________________________________________________________________________________________________________________________________________________________________________________________________
-- BEGIN Q6

SELECT month, (promoters / total_responses - detractors / total_responses) * 100 AS NPS
FROM (SELECT MONTH(response_time) AS month, 
	  SUM(IF(promoter_score >= 9, 1, 0)) AS promoters,
	  SUM(IF(promoter_score <= 6, 1, 0)) AS detractors,
      COUNT(promoter_score) AS total_responses
	  FROM survey_response
	  GROUP BY month
	  ORDER BY month) AS nps_metrics;

-- END Q6
-- ____________________________________________________________________________________________________________________________________________________________________________________________________________
-- BEGIN Q7

SELECT COUNT(first_call_resolution) / COUNT(DISTINCT call_ref) * 100 AS tatjanas_enhanced_participation
FROM staff
INNER JOIN profile ON staff.id = profile.staff_id
INNER JOIN call_record AS cr ON profile.profile_ref LIKE cr.profile_ref
LEFT JOIN survey_response AS sr ON cr.survey_response_id = sr.id
WHERE call_leg = 1
GROUP BY first_name, last_name
HAVING first_name LIKE 'Tatjana' AND last_name LIKE 'Pryor';

-- END Q7
-- ____________________________________________________________________________________________________________________________________________________________________________________________________________
-- BEGIN Q8

SELECT first_name, last_name, FCR_no_count
FROM (SELECT first_name, last_name, SUM(IF(first_call_resolution = 2, 1, 0))  AS FCR_no_count, SUM(IF(first_call_resolution != 2, 1, 0)) AS FCR_other_count
	  FROM staff 
      INNER JOIN profile ON staff.id = profile.staff_id 
      INNER JOIN survey_response AS sr on profile.profile_ref LIKE sr.profile_ref
      WHERE response_time >= '20210601' AND response_time <= '20210617' 
      GROUP BY first_name, last_name) AS FCR_counts
WHERE FCR_no_count = (SELECT MIN(FCR_no_count) 
					  FROM (SELECT SUM(IF(first_call_resolution = 2, 1, 0))  AS FCR_no_count, SUM(IF(first_call_resolution != 2, 1, 0)) AS FCR_other_count
							FROM staff 
							INNER JOIN profile ON staff.id = profile.staff_id 
							INNER JOIN survey_response AS sr on profile.profile_ref LIKE sr.profile_ref
							WHERE response_time >= '20210601' AND response_time <= '20210617' 
							GROUP BY first_name, last_name) AS FCR_counts
					  WHERE FCR_other_count > 0);

-- END Q8
-- ____________________________________________________________________________________________________________________________________________________________________________________________________________
-- BEGIN Q9

SELECT * 
FROM (SELECT AVG(agent_quality) AS averageAQ_teamleader_involved
	  FROM call_record AS cr
	  NATURAL JOIN (SELECT DISTINCT call_ref 
	                FROM profile 
				    INNER JOIN call_record AS cr ON profile.profile_ref LIKE cr.profile_ref
					INNER JOIN role ON profile.role_id = role.id
					WHERE role_id = (SELECT id 
									 FROM role
									 WHERE role_name LIKE 'team Lead')) AS team_lead_calls
	  INNER JOIN survey_response AS sr ON cr.survey_response_id = sr.id) AS averageAQ_teamleader_involved
CROSS JOIN (SELECT AVG(agent_quality) AS averageAQ_overall
	        FROM profile
		    INNER JOIN survey_response AS sr ON profile.profile_ref LIKE sr.profile_ref) AS averageAQ_overall;
		
-- END Q9
-- ____________________________________________________________________________________________________________________________________________________________________________________________________________
-- BEGIN Q10

SELECT first_name, last_name
FROM staff
INNER JOIN profile ON staff.id = profile.staff_id 
INNER JOIN team ON profile.team_id = team.id
WHERE last_name NOT IN (SELECT last_name
					    FROM staff
						INNER JOIN profile ON staff.id = profile.staff_id 
						INNER JOIN team ON profile.team_id = team.id
						WHERE team_name IN (SELECT team_name
											FROM team
									        INNER JOIN profile ON team.id = profile.team_id
											INNER JOIN staff ON profile.staff_id = staff.id
										    WHERE last_name LIKE 'Pryor'))
GROUP BY first_name, last_name
HAVING COUNT(DISTINCT team_name) = (SELECT COUNT(DISTINCT team_name) FROM team WHERE has_staff = 1) 
								 - (SELECT COUNT(DISTINCT team_name)
									FROM team
									INNER JOIN profile ON team.id = profile.team_id
									INNER JOIN staff ON profile.staff_id = staff.id
									WHERE last_name LIKE 'Pryor');

-- END Q10
-- ____________________________________________________________________________________________________________________________________________________________________________________________________________
-- END OF ASSIGNMENT Do not write below this line