# Theoretical notes

One feature of RDS sampling is the oversampling of high-degree nodes.

+ this means processes of cultural diffusion of artifacts, practices, etc. will be represented well, because high-degree nodes will be disproportionately good representations of such artifacts / facts
+ so if we're interested in reasoning processes which are mediated by social diffusion, it's possible that RDS (or snowball sampling in general) will be an astoundingly good method, right?
+ would simulating this process generate annoyingly obvious results?

we've implemented OLS. 
what other social processes of feature generation would be interesting?

+ social diffusion
+ social anti-diffusion (hipster culture)
+ mass media consumption as an outlet of diffusion
+ generation based on attributes
+ generation based on sb-population or family
+ what about communication between parents?

# Algorithmic notes

+ building the social network (should mimic properties of real world networks)
	+ clustering (triadic closure)
	+ focused formation of social ties (Feld)
	+ systematic discrimination / spatial segregation
	+ meeting based on spatial separation
	+ how to know whether we've done a good job at modeling the social network?
+ social learning
+ recruitment process
	+ what do decision theorists think?
	+ find qualitative studies of similar phenomena, or of RDS itself
	+ how is a person located by recruiters?
	+ individuals gain information about the study or have an impression of it, and pass this on to those who they recruit. this perception alters their expectations, and probability of completing the study
	+ simulate hawking as described in the Scott article
+ biases
	+ coupon receiver lives far away from the interview site
	+ coupon receiver is too busy, and can't go.
	+ all other reported reasons reported for not coming in
	+ are people aware or not of their own chance of completing the study?
	+ assume individuals answer questions perfectly
	+ limited knowledge of their own social network
+ estimation
	+ we can get point estimates of every attribute of the individuals
	+ because it's simulation, we can "administer" unreasonably long surveys
	+ race, sex, gender, SES, etc. etc.
	+ social network
		+ reporting bias?
	+ variance of point estimates for continuous variables?
	+ estimates of parameters of underlying (correctly specified) processes
	+ then systematically explore areas of the parameter space where estimation is problematic 
	+ I should use the RDS package which already exists in R for estimation

+ make sure to make all code readable and available to all