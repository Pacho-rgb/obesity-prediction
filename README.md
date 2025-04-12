# obesity-prediction
Model Comparison
a) Goodness of Fit Test
Metric	Full Model	Stepwise Model	Interpretation
McFadden's R²	0.6729048	0.6286989	Both values indicate an excellent fit of the model (both have 60%+) over null model. The small drop shows that the variables removed contributed only a modest amount of the explanatory power.
AIC (Akaike Information Criterion)	66.47	51.128	Stepwise model has a lower AIC indicating that it is more parsimonious and less likely to overfit, hence a better model.
AUC (Area Under Curve)	0.9674	0.9545	Both models are exceptionally strong predictors (AUC > 0.95). The tiny AUC drop (-1.3%) is outweighed by the benefits of a simpler model (fewer variables, easier implementation).
BIC (Bayesian Information Criterion)	108.15253
	66.75932
	Similar to AIC but prioritizes simplicity more strictly. Full model is disfavored due to excessive complexity (many non-significant predictors).
			
