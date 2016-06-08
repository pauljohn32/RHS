PJ 20160606

There are so many of these I can't keep track of what features
are emphasized in which one.  Lacking a better way, here are
synopses.


Highlights

Ex-03.1-neighborhood:
			random intercept: neighid
			No exciting graph, just xtmixed fits, mle

Ex-03.4-growth: jaw bone growth data
				idnr: grouping in random intercept model.
				Spaghetti plot (keyword: "ascending")

Ex-03.4-pups: Rat pups, the size of the litter as predictor
			  Outcome is pup weight, "dam" as fixed effect
			  Recoding with egen
			  twoway graph illustrates dosages

Ex-04.1-gcse: London inner schools
			  spaghetti plots
			  trellis plots
			  Shows recoding for "empty" factor combinations.

Ex-04.2-hsb: This is the "High School and Beyond"
			 Raudenbush & Bryk
	

Ex-04.3-homework: caterpillar plot, shows re standard errors
				  spaghetti plot

Ex-04.4-wheat: The "cleanest" random slope data I've ever seen.
			   trellis, twoway graph of predicted/observed

Ex-04.5-army: Bliese's Army job satisfaction data
			  foreach recode
			  demonstrates xtreg "be" foundations 


Ex-05.1-taxprep:  definition of xtmixed "re" "be" "fe" estimators
                  foreach recoding
				  This has demonstration that be and fe can be
				  reproduced by recode and refit.

Ex-05.2-antisocial: mixed, lrtest
					calculates icc
					lincom
					
Ex-05.4-ezunem:
        		foreach recode
				 logged data
				 lagged predictor
				 xtivreg: Instrumental variables model Anderson Hsiao model


Ex-11.1-respiratory: ordinal outcome
					 gllamm fitter, predict demo
					 reshape to long i and j
					 	meglm
						twoway: multiline graph
				Graph Save code: translate @Graph "graph1.pdf", name("Graph")
				margins for predicted values

Ex-12.2-aggression: meglm ordinal and gllamm compared
					lrtest
					no graphs

Ex-11.3-tvspors: smoking data
				 meglm ordinal, covar unstructured

Ex-11.4-essays: grade categorized 1 2 3 4
				meglm ordinal
