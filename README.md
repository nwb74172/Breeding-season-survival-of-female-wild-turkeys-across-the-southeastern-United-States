# Breeding-season-survival-of-female-wild-turkeys-across-the-southeastern-United-States
Code to reproduce Breeding season survival of female wild turkeys across the southeastern United States; Bakner et al.

	-> code (r code)

	   - model1 (first known fate model outlined in manuscript) 
           - model2 (auto-regressive known fate model outlined in manuscript)

		-> results (r code to reproduce results section of manuscript)
		   - 1sampleSizes (reported sample sizes in results paragraph 1)
		   - 2spatialTemporalVar (reported spatial and temporal variance in results paragraph 2)
		   - 3breedingSeasonSur (reported breeding season survival in paragraph 2)
		   - 4nestApts (reported breeding season survival for multiple nesting attempts in results paragraph 2)
		   - 5dsp (reported daily survival probabilities in results paragraph 3)
		   - 6temporalMods (reported daily survival probabilities in results paragraph 4)

	-> data (data files used to fit models)
	   - model1data (data to fit first known fate model outlined in manuscript)
	   - model2data (data to fit auto-regressive known fate model outlined in manuscript)

	-> jagsScripts (jags scripts used to fit models)
	   - m1.jags (jags script to fit first known fate model outlined in manuscript)
	   - m2.jags (jags script to fit auto-regressive known fate model outlined in manuscript)      

	-> out (r output including model output, table, and figures)
