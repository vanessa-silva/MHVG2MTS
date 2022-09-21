# *MHVG2MTS*: Multilayer Horizontal Visibility Graphs to Multivariate Time Series Analysis

This is the original implementation corresponding to the DGP analysis presented in the paper "*MHVG2MTS: Multilayer Horizontal Visibility Graphs to Multivariate Time Series Analysis*".


## Requirements
- R (>= 3.5.0)


## Configurations

### Data
- All data sets can be found in folder ***data/***.
	- **Data Generating Processes** (DGP), the set of *6* *linear* and *nonlinear* bivariate time series models
		- **bwn_models**: White Noise models, the independent and correlated bivariate time series processes
		- **var_models**: VAR(1) models, the weak and strong bivariate autoregressive processes
		- **garch_models**: GARCH(1,1) models, the weak and strong generalized autoregressive conditionally heteroscedastic processes
		- the GDP are stored in *.RData* files and are in the following format:
			- list of matrix of *ts* objects, ie. *mts*, for each DGP
		- the *csv* files contains an instance example of each respective bivariate time series models
- All the measures computed from MHVG's of the DGP's can be found in frolder ***results/***.
	- **DegreeSeq**, the intra/inter/all-layer degree sequences and the ratio degree sequences
	- **DegreeDistribution**, the intra/inter/all-layer degree distributions and respective the mean's and sd's degree distributions
	- **GlobalFeatures**, the intra/inter/all-layer global feature vectors (average degree, average path length, number of communities and modularity) and the relational feature vectors (average ratio degree and intra/inter/all-layer Jensen–Shannon divergense)
- All the empirical results also can be found in frolder ***results/***.
	- **PCA_results**, the intra/inter/all-layer global features and the relationa features PCA analysis
	- **Clustering_results**, the clustering analysis the global and relations feature vectors

### Source Files
- **libraries** : contains all required packages
- **aux_code/** : contains all auxiliary functions
- **info_data** : contains some auxiliary data informations
- **main_DGP** : contains data parameters and runs the procedures to simulate Data Generation Processes
- **main_bts** : contains procedures to analyse ACF's and CCF's of the DGP
- **main_local_features** : runs the main procedures for the empirical evaluation of degree distributions of *DGP*
- **main_global_features** :  runs the main procedures for the empirical evaluation of global and relations features of DGP
- **main_clustering** : runs the main procedures for the experimental evaluation of DGP clustering based of MNet features
