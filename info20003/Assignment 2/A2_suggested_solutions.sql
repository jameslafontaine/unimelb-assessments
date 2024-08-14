
/*
 Below are some suggested solutions for A2.
 These are not the ONLY correct solutions, others may have been accepted. 
*/

-- Queries were tested against a modified dataset (available on LMS), and with the following sql mode set:
SET @@SQL_MODE = 'ONLY_FULL_GROUP_BY,STRICT_TRANS_TABLES,NO_ZERO_IN_DATE,NO_ZERO_DATE,ERROR_FOR_DIVISION_BY_ZERO,NO_ENGINE_SUBSTITUTION';

/* 
	(1) Display the first and last names of all the team leads. Do not repeat names if a staff member has been a team lead more than once.
*/

SELECT DISTINCT s.first_name, s.last_name
FROM profile p
INNER JOIN staff s ON p.staff_id=s.id
INNER JOIN `role` r ON p.role_id=r.id
WHERE r.role_name = 'Team Lead';


/*
	(2) Show all staff who have ever worked for a Victorian team. Show their name and team.  
    Display the list sorted alphabetically by last name.
*/

SELECT DISTINCT s.last_name, s.first_name, t.team_name
FROM profile p
INNER JOIN team t ON p.team_id=t.id
INNER JOIN staff s ON p.staff_id=s.id
INNER JOIN team parent_team ON t.parent_id = parent_team.id
WHERE parent_team.team_name = 'Victoria'
ORDER BY s.last_name;

/*	(3) Show staff who worked in Errinundra at any time between 13th April and 13th May.
        Display their first and last name
*/

SELECT DISTINCT s.first_name, s.last_name
FROM profile p
INNER JOIN staff s ON p.staff_id=s.id
INNER JOIN team t ON p.team_id=t.id
WHERE t.team_name = 'Errinundra'
  AND (p.valid_until >= '2021-04-13')  AND  (p.valid_from <= '2021-05-13') ;

/*
	(4) Which agents (Not including team leaders) were working only in Werrikimbe (and no other team) on 23th of May? Show their name.
*/

SELECT s.first_name, s.last_name
FROM profile p
INNER JOIN `role` r ON p.role_id=r.id
INNER JOIN team t ON p.team_id=t.id
INNER JOIN staff s ON p.staff_id=s.id
WHERE '2021-05-23' BETWEEN p.valid_from AND p.valid_until
  AND t.team_name = 'Werrikimbe'
  AND r.role_name = 'Agent'
  AND p.staff_id NOT IN (
		SELECT p.staff_id
		FROM profile p
		INNER JOIN `role` r ON p.role_id=r.id
		INNER JOIN team t ON p.team_id=t.id
		WHERE '2021-05-23' BETWEEN p.valid_from AND p.valid_until
		  -- AND r.role_name = 'Agent' -- allow for both with/without this condition as correct, since question is slightly ambiguous
		GROUP BY p.staff_id
		HAVING COUNT(DISTINCT p.team_id) > 1
  );


/*
	(5) Calculate the monthly average agent quality (AQ) score for each team. 
    Display the team name, month and average AQ sorted by average AQ so that the 
    highest scoring teams are listed first.
*/

SELECT t.team_name,
	   month(sr.response_time) AS month,
	   AVG(sr.agent_quality) AS avgAQ
FROM survey_response sr
INNER JOIN profile p ON p.profile_ref = sr.profile_ref
INNER JOIN team t ON t.id = p.team_id
GROUP BY t.team_name, month(sr.response_time)
ORDER BY AVG(sr.agent_quality) DESC;

/*
	(6) The business want to know how their Net Promoter Score (NPS) is tracking.  
    Calculate the NPS of the organisation for the months of April, May and June 2021. Display the month and NPS value sorted in increasing order of month.
*/

SELECT month(response_time) month,
	   ( SUM( CASE WHEN (sr.promoter_score IN (9,10)) THEN 1 ELSE 0 END ) / COUNT(sr.promoter_score) * 100 ) -		/* % of promoters */
       ( SUM( CASE WHEN (sr.promoter_score IN (0,1,2,3,4,5,6)) THEN 1 ELSE 0 END ) / COUNT(sr.promoter_score) * 100 ) /* % of detractors */
       AS NPS
FROM survey_response sr
GROUP BY month(sr.response_time)
ORDER BY month(sr.response_time);

/*
	(7) "Enhanced" participation is similar to participation, but considers not just offered surveys, but offered surveys with
    at least one question - the first - answered by the customer.	
*/

SELECT ROUND( COUNT(first_call_resolution) / count(DISTINCT cr.call_ref) * 100, 0) AS enh_partRate
FROM call_record cr
INNER JOIN profile p ON p.profile_ref = cr.profile_ref
INNER JOIN staff s ON p.staff_id=s.id
LEFT JOIN survey_response sr on sr.id = cr.survey_response_id
WHERE cr.call_time BETWEEN '2021-06-01 00:00:00' AND '2021-06-30 23:59:59'
  #allow students to use WHERE staff.id = 2 since we do this in other questions.
  AND s.first_name = 'Tatjana'
  AND s.last_name = 'Pryor'
  AND call_leg = 1
;

-- NOTE: using cr.call_time instead of sr.response_time is also OK, gives a very different answer depending on dataset.
SELECT ROUND( SUM( CASE WHEN (sr.first_call_resolution IS NOT NULL) THEN 1 ELSE 0 END ) / count(DISTINCT cr.call_ref) * 100, 0) AS enh_partRate
FROM call_record cr
INNER JOIN profile p ON p.profile_ref = cr.profile_ref
INNER JOIN staff s ON p.staff_id=s.id
LEFT JOIN survey_response sr on sr.id = cr.survey_response_id
WHERE sr.response_time BETWEEN '2021-06-01 00:00:00' AND '2021-06-30 23:59:59'
  #allow students to use WHERE staff.id = 2 since we do this in other questions.
  AND s.first_name = 'Tatjana'
  AND s.last_name = 'Pryor'
  AND call_leg = 1
;


/*
	(8) What agent has the lowest number of FCR="no" survey responses in the period 01 June - 17 June? If there are multiple agents with equal values, return them all. In case of a tie, display all agents. Display agents' first and last name along with their FCR count.

*/

SELECT s.first_name, s.last_name,
       COUNT(sr.id) AS fcr_no
FROM survey_response sr
INNER JOIN profile p ON p.profile_ref = sr.profile_ref
INNER JOIN staff s ON s.id = p.staff_id
WHERE sr.response_time BETWEEN '2021-06-01 00:00:00' AND '2021-06-17 23:59:59'
  AND sr.first_call_resolution = 2
GROUP BY s.first_name, s.last_name
HAVING fcr_no <= ALL (
	SELECT COUNT(sr1.id)
    FROM survey_response sr1
    INNER JOIN profile p1 ON p1.profile_ref = sr1.profile_ref
    WHERE sr1.response_time BETWEEN '2021-06-01 00:00:00' AND '2021-06-17 23:59:59'
	  AND sr1.first_call_resolution = 2
    GROUP BY p1.staff_id
)
ORDER BY fcr_no;


/*
	(9) Show the average AQ score for interactions where the agent has needed to contact a team leader during the interaction compared to the overall average AQ. 
		Display the average score with team leader involved and overall average AQ. 
*/

SELECT AVG(sr.agent_quality) AS avgAQwithTL,
		(SELECT AVG(sr.agent_quality) FROM survey_response sr) AS avgAQOverallTL
FROM call_record cr
INNER JOIN survey_response sr on cr.survey_response_id = sr.id
WHERE cr.call_ref IN (
	/* all call_ref of multi-leg calls involving a Team Lead */
	SELECT DISTINCT cr1.call_ref
	FROM call_record cr1
	INNER JOIN profile p ON p.profile_ref = cr1.profile_ref
	INNER JOIN role r ON p.role_id=r.id
	WHERE cr1.call_leg > 1
	  AND r.role_name = 'Team Lead'
)
  AND cr.call_leg = 1;
  
  
  /*  (10) "Wait, you've never met Tatjana?" 
    Find the names of staff members that have worked in ALL the teams EXCEPT for the teams that 'Tatjana Pryor' (staffid = 2) has worked in.
    Only consider teams which 'have staff' (ie has_staff =1)
  */
  
Select first_name, last_name 
From staff as s1 inner join profile on s1.id = profile.staff_id
Where not exists (
	select team_id
    from profile
    where staff_id = s1.id
	and team_id in 
		(Select team_id
		From team inner join profile on profile.team_id = team.id
		where staff_id = 2)
)
Group by id
having count(distinct team_id) = (
	Select count(id)
	From team 
    Where has_staff = 1
) - (
	Select count(distinct team_id)
	From profile
	where staff_id = 2
)
;

  
  

