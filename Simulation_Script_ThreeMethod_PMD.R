# This is the code for the simulation of the efficiancy of the 2-MM and 3-MM designs


# load simsem package and set working directory
library(simsem)


##### define two-method measurement population models#####

# 2M-valid[.4]
TwoMethod_popModel_5_4 <- "
#define loadings
smoking =~ 0.5*s1 + 0.5*s2 + 0.7*o1 + 0.7*o2
bias_s =~ 0.5*s1 + 0.5*s2
health =~ 0.7*h1 + 0.7*h2 + 0.7*h3 + 0.7*h4
#define variances
smoking ~~ 1*smoking
bias_s ~~ 1*bias_s
health ~~ 1*health
#define covariances/correlations
smoking ~~ 0*bias_s
health ~~ 0*bias_s
health ~~ (-0.4)*smoking
#define residual variances of items
s1 ~~ 0.5*s1
s2 ~~ 0.5*s2
o1 ~~ 0.51*o1
o2 ~~ 0.51*o2
h1 ~~ 0.51*h1
h2 ~~ 0.51*h2
h3 ~~ 0.51*h3
h4 ~~ 0.51*h4
"

# 2M-biased[.4]
TwoMethod_popModel_3_4 <- "
#define loadings
smoking =~ 0.35*s1 + 0.35*s2 + 0.7*o1 + 0.7*o2
bias_s =~ 0.61*s1 + 0.61*s2
health =~ 0.7*h1 + 0.7*h2 + 0.7*h3 + 0.7*h4
#define variances
smoking ~~ 1*smoking
bias_s ~~ 1*bias_s
health ~~ 1*health
#define covariances/correlations
smoking ~~ 0*bias_s
health ~~ 0*bias_s
health ~~ (-0.4)*smoking
#define residual variances of items
s1 ~~ (1-(0.35)^2-(0.61)^2)*s1
s2 ~~ (1-(0.35)^2-(0.61)^2)*s2
o1 ~~ 0.51*o1
o2 ~~ 0.51*o2
h1 ~~ 0.51*h1
h2 ~~ 0.51*h2
h3 ~~ 0.51*h3
h4 ~~ 0.51*h4
"

# 2M-valid[.1]
TwoMethod_popModel_5_1 <- "
#define loadings
smoking =~ 0.5*s1 + 0.5*s2 + 0.7*o1 + 0.7*o2
bias_s =~ 0.5*s1 + 0.5*s2
health =~ 0.7*h1 + 0.7*h2 + 0.7*h3 + 0.7*h4
#define variances
smoking ~~ 1*smoking
bias_s ~~ 1*bias_s
health ~~ 1*health
#define covariances/correlations
smoking ~~ 0*bias_s
health ~~ 0*bias_s
health ~~ (-0.1)*smoking
#define residual variances of items
s1 ~~ 0.5*s1
s2 ~~ 0.5*s2
o1 ~~ 0.51*o1
o2 ~~ 0.51*o2
h1 ~~ 0.51*h1
h2 ~~ 0.51*h2
h3 ~~ 0.51*h3
h4 ~~ 0.51*h4
"

# 2M-biased[.1]
TwoMethod_popModel_3_1 <- "
#define loadings
smoking =~ 0.35*s1 + 0.35*s2 + 0.7*o1 + 0.7*o2
bias_s =~ 0.61*s1 + 0.61*s2
health =~ 0.7*h1 + 0.7*h2 + 0.7*h3 + 0.7*h4
#define variances
smoking ~~ 1*smoking
bias_s ~~ 1*bias_s
health ~~ 1*health
#define covariances/correlations
smoking ~~ 0*bias_s
health ~~ 0*bias_s
health ~~ (-0.1)*smoking
#define residual variances of items
s1 ~~ (1-(0.35)^2-(0.61)^2)*s1
s2 ~~ (1-(0.35)^2-(0.61)^2)*s2
o1 ~~ 0.51*o1
o2 ~~ 0.51*o2
h1 ~~ 0.51*h1
h2 ~~ 0.51*h2
h3 ~~ 0.51*h3
h4 ~~ 0.51*h4
"


##### define three-method measurement population models#####

# 3M-valid-common[.4]
ThreeMethod_popModel_55_4_5 <- "
#define factor loadings
smoking =~ 0.5*p1 + 0.5*p2 + 0.5*s1 + 0.5*s2 + 0.7*o1 + 0.7*o2
bias_p =~ 0.5*p1 + 0.5*p2
bias_s =~ 0.5*s1 + 0.5*s2
health =~ 0.7*h1 + 0.7*h2 + 0.7*h3 + 0.7*h4
#define variances
smoking ~~ 1*smoking
bias_p ~~ 1*bias_p
bias_s ~~ 1*bias_s
health ~~ 1*health
#Kodefine variances
smoking ~~ 0*bias_p
smoking ~~ 0*bias_s
health ~~ 0*bias_p
health ~~ 0*bias_s
health ~~ (-0.4)*smoking
bias_p ~~ 0.5*bias_s
#define residual variances of items
p1 ~~ 0.5*p1
p2 ~~ 0.5*p2
s1 ~~ 0.5*s1
s2 ~~ 0.5*s2
o1 ~~ 0.51*o1
o2 ~~ 0.51*o2
h1 ~~ 0.51*h1
h2 ~~ 0.51*h2
h3 ~~ 0.51*h3
h4 ~~ 0.51*h4
"

# 3M-mixed-common[.4]

ThreeMethod_popModel_35_4_5 <- "
#define factor loadings
smoking =~ 0.35*p1 + 0.35*p2 + 0.5*s1 + 0.5*s2 + 0.7*o1 + 0.7*o2
bias_p =~ 0.61*p1 + 0.61*p2
bias_s =~ 0.5*s1 + 0.5*s2
health =~ 0.7*h1 + 0.7*h2 + 0.7*h3 + 0.7*h4
#define variances
smoking ~~ 1*smoking
bias_p ~~ 1*bias_p
bias_s ~~ 1*bias_s
health ~~ 1*health
#Kodefine variances
smoking ~~ 0*bias_p
smoking ~~ 0*bias_s
health ~~ 0*bias_p
health ~~ 0*bias_s
health ~~ (-0.4)*smoking
bias_p ~~ 0.5*bias_s
#define residual variances of items
p1 ~~ (1-(0.35)^2-(0.61)^2)*p1
p2 ~~ (1-(0.35)^2-(0.61)^2)*p2
s1 ~~ 0.5*s1
s2 ~~ 0.5*s2
o1 ~~ 0.51*o1
o2 ~~ 0.51*o2
h1 ~~ 0.51*h1
h2 ~~ 0.51*h2
h3 ~~ 0.51*h3
h4 ~~ 0.51*h4
"

# 3M-biased-common[.4]
ThreeMethod_popModel_33_4_5 <- "
#define factor loadings
smoking =~ 0.35*p1 + 0.35*p2 + 0.35*s1 + 0.35*s2 + 0.7*o1 + 0.7*o2
bias_p =~ 0.61*p1 + 0.61*p2
bias_s =~ 0.61*s1 + 0.61*s2
health =~ 0.7*h1 + 0.7*h2 + 0.7*h3 + 0.7*h4
#define variances
smoking ~~ 1*smoking
bias_p ~~ 1*bias_p
bias_s ~~ 1*bias_s
health ~~ 1*health
#Kodefine variances
smoking ~~ 0*bias_p
smoking ~~ 0*bias_s
health ~~ 0*bias_p
health ~~ 0*bias_s
health ~~ (-0.4)*smoking
bias_p ~~ 0.5*bias_s
#define residual variances of items
p1 ~~ (1-(0.35)^2-(0.61)^2)*p1
p2 ~~ (1-(0.35)^2-(0.61)^2)*p2
s1 ~~ (1-(0.35)^2-(0.61)^2)*s1
s2 ~~ (1-(0.35)^2-(0.61)^2)*s2
o1 ~~ 0.51*o1
o2 ~~ 0.51*o2
h1 ~~ 0.51*h1
h2 ~~ 0.51*h2
h3 ~~ 0.51*h3
h4 ~~ 0.51*h4
"

# 3M-valid-Unique[.4]

ThreeMethod_popModel_55_4_1 <- "
#define factor loadings
smoking =~ 0.5*p1 + 0.5*p2 + 0.5*s1 + 0.5*s2 + 0.7*o1 + 0.7*o2
bias_p =~ 0.5*p1 + 0.5*p2
bias_s =~ 0.5*s1 + 0.5*s2
health =~ 0.7*h1 + 0.7*h2 + 0.7*h3 + 0.7*h4
#define variances
smoking ~~ 1*smoking
bias_p ~~ 1*bias_p
bias_s ~~ 1*bias_s
health ~~ 1*health
#Kodefine variances
smoking ~~ 0*bias_p
smoking ~~ 0*bias_s
health ~~ 0*bias_p
health ~~ 0*bias_s
health ~~ (-0.4)*smoking
bias_p ~~ 0.1*bias_s
#define residual variances of items
p1 ~~ 0.5*p1
p2 ~~ 0.5*p2
s1 ~~ 0.5*s1
s2 ~~ 0.5*s2
o1 ~~ 0.51*o1
o2 ~~ 0.51*o2
h1 ~~ 0.51*h1
h2 ~~ 0.51*h2
h3 ~~ 0.51*h3
h4 ~~ 0.51*h4
"

# 3M-mixed-Unique[.4]
ThreeMethod_popModel_35_4_1 <- "
#define factor loadings
smoking =~ 0.35*p1 + 0.35*p2 + 0.5*s1 + 0.5*s2 + 0.7*o1 + 0.7*o2
bias_p =~ 0.61*p1 + 0.61*p2
bias_s =~ 0.5*s1 + 0.5*s2
health =~ 0.7*h1 + 0.7*h2 + 0.7*h3 + 0.7*h4
#define variances
smoking ~~ 1*smoking
bias_p ~~ 1*bias_p
bias_s ~~ 1*bias_s
health ~~ 1*health
#Kodefine variances
smoking ~~ 0*bias_p
smoking ~~ 0*bias_s
health ~~ 0*bias_p
health ~~ 0*bias_s
health ~~ (-0.4)*smoking
bias_p ~~ 0.1*bias_s
#define residual variances of items
p1 ~~ (1-(0.35)^2-(0.61)^2)*p1
p2 ~~ (1-(0.35)^2-(0.61)^2)*p2
s1 ~~ 0.5*s1
s2 ~~ 0.5*s2
o1 ~~ 0.51*o1
o2 ~~ 0.51*o2
h1 ~~ 0.51*h1
h2 ~~ 0.51*h2
h3 ~~ 0.51*h3
h4 ~~ 0.51*h4
"

# 3M-biased-Unique[.4]
ThreeMethod_popModel_33_4_1 <- "
#define factor loadings
smoking =~ 0.35*p1 + 0.35*p2 + 0.35*s1 + 0.35*s2 + 0.7*o1 + 0.7*o2
bias_p =~ 0.61*p1 + 0.61*p2
bias_s =~ 0.61*s1 + 0.61*s2
health =~ 0.7*h1 + 0.7*h2 + 0.7*h3 + 0.7*h4
#define variances
smoking ~~ 1*smoking
bias_p ~~ 1*bias_p
bias_s ~~ 1*bias_s
health ~~ 1*health
#Kodefine variances
smoking ~~ 0*bias_p
smoking ~~ 0*bias_s
health ~~ 0*bias_p
health ~~ 0*bias_s
health ~~ (-0.4)*smoking
bias_p ~~ 0.1*bias_s
#define residual variances of items
p1 ~~ (1-(0.35)^2-(0.61)^2)*p1
p2 ~~ (1-(0.35)^2-(0.61)^2)*p2
s1 ~~ (1-(0.35)^2-(0.61)^2)*s1
s2 ~~ (1-(0.35)^2-(0.61)^2)*s2
o1 ~~ 0.51*o1
o2 ~~ 0.51*o2
h1 ~~ 0.51*h1
h2 ~~ 0.51*h2
h3 ~~ 0.51*h3
h4 ~~ 0.51*h4
"


# 3M-valid-common[.1]

ThreeMethod_popModel_55_1_5 <- "
#define factor loadings
smoking =~ 0.5*p1 + 0.5*p2 + 0.5*s1 + 0.5*s2 + 0.7*o1 + 0.7*o2
bias_p =~ 0.5*p1 + 0.5*p2
bias_s =~ 0.5*s1 + 0.5*s2
health =~ 0.7*h1 + 0.7*h2 + 0.7*h3 + 0.7*h4
#define variances
smoking ~~ 1*smoking
bias_p ~~ 1*bias_p
bias_s ~~ 1*bias_s
health ~~ 1*health
#define covariances/correlations
smoking ~~ 0*bias_p
smoking ~~ 0*bias_s
health ~~ 0*bias_p
health ~~ 0*bias_s
health ~~ (-0.1)*smoking
bias_p ~~ 0.5*bias_s
#define residual variances of items
p1 ~~ 0.5*p1
p2 ~~ 0.5*p2
s1 ~~ 0.5*s1
s2 ~~ 0.5*s2
o1 ~~ 0.51*o1
o2 ~~ 0.51*o2
h1 ~~ 0.51*h1
h2 ~~ 0.51*h2
h3 ~~ 0.51*h3
h4 ~~ 0.51*h4
"

# 3M-mixed-common[.1]
ThreeMethod_popModel_35_1_5 <- "
#define factor loadings
smoking =~ 0.35*p1 + 0.35*p2 + 0.5*s1 + 0.5*s2 + 0.7*o1 + 0.7*o2
bias_p =~ 0.61*p1 + 0.61*p2
bias_s =~ 0.5*s1 + 0.5*s2
health =~ 0.7*h1 + 0.7*h2 + 0.7*h3 + 0.7*h4
#factor variances
smoking ~~ 1*smoking
bias_p ~~ 1*bias_p
bias_s ~~ 1*bias_s
health ~~ 1*health
#define covariances/correlations
smoking ~~ 0*bias_p
smoking ~~ 0*bias_s
health ~~ 0*bias_p
health ~~ 0*bias_s
health ~~ (-0.1)*smoking
bias_p ~~ 0.5*bias_s
#define residual variances of items
p1 ~~ (1-(0.35)^2-(0.61)^2)*p1
p2 ~~ (1-(0.35)^2-(0.61)^2)*p2
s1 ~~ 0.5*s1
s2 ~~ 0.5*s2
o1 ~~ 0.51*o1
o2 ~~ 0.51*o2
h1 ~~ 0.51*h1
h2 ~~ 0.51*h2
h3 ~~ 0.51*h3
h4 ~~ 0.51*h4
"

# 3M-biased-common[.1]
ThreeMethod_popModel_33_1_5 <- "
#define factor loadings
smoking =~ 0.35*p1 + 0.35*p2 + 0.35*s1 + 0.35*s2 + 0.7*o1 + 0.7*o2
bias_p =~ 0.61*p1 + 0.61*p2
bias_s =~ 0.61*s1 + 0.61*s2
health =~ 0.7*h1 + 0.7*h2 + 0.7*h3 + 0.7*h4
#factor variances
smoking ~~ 1*smoking
bias_p ~~ 1*bias_p
bias_s ~~ 1*bias_s
health ~~ 1*health
#define covariances/correlations
smoking ~~ 0*bias_p
smoking ~~ 0*bias_s
health ~~ 0*bias_p
health ~~ 0*bias_s
health ~~ (-0.1)*smoking
bias_p ~~ 0.5*bias_s
#define residual variances of items
p1 ~~ (1-(0.35)^2-(0.61)^2)*p1
p2 ~~ (1-(0.35)^2-(0.61)^2)*p2
s1 ~~ (1-(0.35)^2-(0.61)^2)*s1
s2 ~~ (1-(0.35)^2-(0.61)^2)*s2
o1 ~~ 0.51*o1
o2 ~~ 0.51*o2
h1 ~~ 0.51*h1
h2 ~~ 0.51*h2
h3 ~~ 0.51*h3
h4 ~~ 0.51*h4
"

# 3M-valid-unique[.1]
ThreeMethod_popModel_55_1_1 <- "
#define factor loadings
smoking =~ 0.5*p1 + 0.5*p2 + 0.5*s1 + 0.5*s2 + 0.7*o1 + 0.7*o2
bias_p =~ 0.5*p1 + 0.5*p2
bias_s =~ 0.5*s1 + 0.5*s2
health =~ 0.7*h1 + 0.7*h2 + 0.7*h3 + 0.7*h4
#factor variances
smoking ~~ 1*smoking
bias_p ~~ 1*bias_p
bias_s ~~ 1*bias_s
health ~~ 1*health
#define covariances/correlations
smoking ~~ 0*bias_p
smoking ~~ 0*bias_s
health ~~ 0*bias_p
health ~~ 0*bias_s
health ~~ (-0.1)*smoking
bias_p ~~ 0.1*bias_s
#define residual variances of items
p1 ~~ 0.5*p1
p2 ~~ 0.5*p2
s1 ~~ 0.5*s1
s2 ~~ 0.5*s2
o1 ~~ 0.51*o1
o2 ~~ 0.51*o2
h1 ~~ 0.51*h1
h2 ~~ 0.51*h2
h3 ~~ 0.51*h3
h4 ~~ 0.51*h4
"

# 3M-mixed-unique[.1]
ThreeMethod_popModel_35_1_1 <- "
#define factor loadings
smoking =~ 0.35*p1 + 0.35*p2 + 0.5*s1 + 0.5*s2 + 0.7*o1 + 0.7*o2
bias_p =~ 0.61*p1 + 0.61*p2
bias_s =~ 0.5*s1 + 0.5*s2
health =~ 0.7*h1 + 0.7*h2 + 0.7*h3 + 0.7*h4
#factor variances
smoking ~~ 1*smoking
bias_p ~~ 1*bias_p
bias_s ~~ 1*bias_s
health ~~ 1*health
#define covariances/correlations
smoking ~~ 0*bias_p
smoking ~~ 0*bias_s
health ~~ 0*bias_p
health ~~ 0*bias_s
health ~~ (-0.1)*smoking
bias_p ~~ 0.1*bias_s
#define residual variances of items
p1 ~~ (1-(0.35)^2-(0.61)^2)*p1
p2 ~~ (1-(0.35)^2-(0.61)^2)*p2
s1 ~~ 0.5*s1
s2 ~~ 0.5*s2
o1 ~~ 0.51*o1
o2 ~~ 0.51*o2
h1 ~~ 0.51*h1
h2 ~~ 0.51*h2
h3 ~~ 0.51*h3
h4 ~~ 0.51*h4
"

# 3M-biased-unique[.1]
ThreeMethod_popModel_33_1_1 <- "
#define factor loadings
smoking =~ 0.35*p1 + 0.35*p2 + 0.35*s1 + 0.35*s2 + 0.7*o1 + 0.7*o2
bias_p =~ 0.61*p1 + 0.61*p2
bias_s =~ 0.61*s1 + 0.61*s2
health =~ 0.7*h1 + 0.7*h2 + 0.7*h3 + 0.7*h4
#factor variances
smoking ~~ 1*smoking
bias_p ~~ 1*bias_p
bias_s ~~ 1*bias_s
health ~~ 1*health
#define covariances/correlations
smoking ~~ 0*bias_p
smoking ~~ 0*bias_s
health ~~ 0*bias_p
health ~~ 0*bias_s
health ~~ (-0.1)*smoking
bias_p ~~ 0.1*bias_s
#define residual variances of items
p1 ~~ (1-(0.35)^2-(0.61)^2)*p1
p2 ~~ (1-(0.35)^2-(0.61)^2)*p2
s1 ~~ (1-(0.35)^2-(0.61)^2)*s1
s2 ~~ (1-(0.35)^2-(0.61)^2)*s2
o1 ~~ 0.51*o1
o2 ~~ 0.51*o2
h1 ~~ 0.51*h1
h2 ~~ 0.51*h2
h3 ~~ 0.51*h3
h4 ~~ 0.51*h4
"


#### define analysis models####

TwoMethod_analyzeModel <- "
smoking =~ s1 + s2 + o1 + o2
# restrict loadings to be equal in order to identify model
bias_s =~ lam_bias*s1 + lam_bias*s2
# set first loading to 1 for scaling
health =~ 1*h1 + h2 + h3 + h4
#define covariances/correlations
smoking ~~ 0*bias_s
health ~~ 0*bias_s
smoking ~~1*smoking
bias_s~~1*bias_s
#regression
health ~ smoking
"

ThreeMethod_analyzeModel <- "
smoking =~ p1+ p2 + s1 + s2 + o1 + o2
# restrict loadings to be equal in order to identify model
bias_s =~ lam_bias_s*s1 + lam_bias_s*s2
bias_p =~ lam_bias_p*p1 + lam_bias_p*p2
# set first loading to 1 for scaling
health =~ lam_h1*h1 + h2 + h3 + h4
#define covariances/correlations
smoking ~~ 0*bias_s
health ~~ 0*bias_s
smoking ~~ 0*bias_p
health ~~ 0*bias_p
smoking ~~var_smoking*smoking
bias_s~~var_biass*bias_s
bias_p~~var_biasp*bias_p
#regression
health ~ smoking
lam_h1 == 1
var_biass == 1
var_biasp == 1
var_smoking == 1
"

#### define missingness patterns ####

#cost-ratio A1
n_cc_ThreeMethod_A1 <- 479
nCheap_ThreeMethod_A1 <- c(500, 600, 700, 800, 900, 1000)
nExpensive_ThreeMethod_A1 <- c(462, 375, 288, 201, 114, 27)
PercentMiss_ThreeMethod_A1 <-
  1 - (nExpensive_ThreeMethod_A1 / nCheap_ThreeMethod_A1)


n_cc_TwoMethod_A <- 625
nCheap_TwoMethod_A <-
  c(700,
    800,
    900,
    1000,
    1100,
    1200,
    1300,
    1400,
    1500,
    1600,
    1700,
    1800,
    1900,
    2000)
nExpensive_TwoMethod_A <-
  c(592, 549, 505, 462, 418, 375, 331, 288, 244, 201, 157, 114, 70, 27)
PercentMiss_TwoMethod_A <-
  1 - (nExpensive_TwoMethod_A / nCheap_TwoMethod_A)


n_cc_ThreeMethod_A2 <- 542
nCheap_ThreeMethod_A2 <-
  c(600, 700, 800, 900, 1000, 1100, 1200, 1300)
nExpensive_ThreeMethod_A2 <-
  c(505, 440, 375, 310, 244, 179, 114, 49)
PercentMiss_ThreeMethod_A2 <-
  1 - (nExpensive_ThreeMethod_A2 / nCheap_ThreeMethod_A2)

n_cc_ThreeMethod_A3 <- 583
nCheap_ThreeMethod_A3 <-
  c(600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1600)
nExpensive_ThreeMethod_A3 <-
  c(575, 521, 468, 414, 361, 307, 253, 200, 146, 92, 39)
PercentMiss_ThreeMethod_A3 <-
  1 - (nExpensive_ThreeMethod_A3 / nCheap_ThreeMethod_A3)

n_cc_ThreeMethod_A4 <- n_cc_TwoMethod_A
nCheap_ThreeMethod_A4 <-nCheap_TwoMethod_A
nExpensive_ThreeMethod_A4<-nExpensive_TwoMethod_A
PercentMiss_ThreeMethod_A4 <-
  1 - (nExpensive_ThreeMethod_A4 / nCheap_ThreeMethod_A4)

#cost-ratio B

n_cc_ThreeMethod_B1 <- 337
nCheap_ThreeMethod_B1 <- c(400, 500, 600, 700, 800, 900, 1000)
nExpensive_ThreeMethod_B1 <- c(307, 258, 210, 161, 112, 64, 15)
PercentMiss_ThreeMethod_B1 <-
  1 - (nExpensive_ThreeMethod_B1 / nCheap_ThreeMethod_B1)

n_cc_TwoMethod_B <- 403
nCheap_TwoMethod_B <-
  c(500,
    600,
    700,
    800,
    900,
    1000,
    1100,
    1200,
    1300,
    1400,
    1500,
    1600,
    1700,
    1800,
    1900,
    2000)
nExpensive_TwoMethod_B <-
  c(380,
    356,
    331,
    307,
    283,
    258,
    234,
    210,
    185,
    161,
    137,
    112,
    88,
    64,
    39,
    15)
PercentMiss_TwoMethod_B <-
  1 - (nExpensive_TwoMethod_B / nCheap_TwoMethod_B)



n_cc_ThreeMethod_B2 <- 368
nCheap_ThreeMethod_B2 <-
  c(400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300)
nExpensive_ThreeMethod_B2 <-
  c(356, 319, 283, 246, 210, 173, 137, 100, 64, 27)
PercentMiss_ThreeMethod_B2 <-
  1 - (nExpensive_ThreeMethod_B2 / nCheap_ThreeMethod_B2)


n_cc_ThreeMethod_B3 <- 385
nCheap_ThreeMethod_B3 <-
  c(400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1600)
nExpensive_ThreeMethod_B3 <-
  c(382, 352, 322, 292, 262, 232, 202, 172, 142, 112, 82, 52, 22)
PercentMiss_ThreeMethod_B3 <-
  1 - (nExpensive_ThreeMethod_B3 / nCheap_ThreeMethod_B3)

n_cc_ThreeMethod_B4 <- n_cc_TwoMethod_B
nCheap_ThreeMethod_B4 <-nCheap_TwoMethod_B
nExpensive_ThreeMethod_B4<-nExpensive_TwoMethod_B
PercentMiss_ThreeMethod_B4 <-
  1 - (nExpensive_ThreeMethod_B4 / nCheap_ThreeMethod_B4)

# cost-ratio C

n_cc_ThreeMethod_C1 <- 171
nCheap_ThreeMethod_C1 <- c(200, 300, 400, 500, 600, 700, 800, 900)
nExpensive_ThreeMethod_C1 <- c(166, 146, 126, 106, 86, 66, 46, 26)
PercentMiss_ThreeMethod_C1 <-
  1 - (nExpensive_ThreeMethod_C1 / nCheap_ThreeMethod_C1)

n_cc_TwoMethod_C <- 187
nCheap_TwoMethod_C <-
  c(
    200,
    300,
    400,
    500,
    600,
    700,
    800,
    900,
    1000,
    1100,
    1200,
    1300,
    1400,
    1500,
    1600,
    1700,
    1800,
    1900
  )
nExpensive_TwoMethod_C <-
  c(186,
    176,
    166,
    156,
    146,
    136,
    126,
    116,
    106,
    96,
    86,
    76,
    66,
    56,
    46,
    36,
    26,
    16)
PercentMiss_TwoMethod_C <-
  1 - (nExpensive_TwoMethod_C / nCheap_TwoMethod_C)


n_cc_ThreeMethod_C2 <- 179
nCheap_ThreeMethod_C2 <-
  c(200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300)
nExpensive_ThreeMethod_C2 <-
  c(176, 161, 146, 131, 116, 101, 86, 71, 56, 41, 26, 11)
PercentMiss_ThreeMethod_C2 <-
  1 - (nExpensive_ThreeMethod_C2 / nCheap_ThreeMethod_C2)


n_cc_ThreeMethod_C3 <- 183
nCheap_ThreeMethod_C3 <-
  c(200,
    300,
    400,
    500,
    600,
    700,
    800,
    900,
    1000,
    1100,
    1200,
    1300,
    1400,
    1500)
nExpensive_ThreeMethod_C3 <-
  c(182, 169, 157, 145, 132, 120, 108, 95, 83, 71, 58, 46, 34, 21)
PercentMiss_ThreeMethod_C3 <-
  1 - (nExpensive_ThreeMethod_C3 / nCheap_ThreeMethod_C3)

n_cc_ThreeMethod_C4 <- n_cc_TwoMethod_C
nCheap_ThreeMethod_C4 <-nCheap_TwoMethod_C
nExpensive_ThreeMethod_C4<-nExpensive_TwoMethod_C
PercentMiss_ThreeMethod_C4 <-
  1 - (nExpensive_ThreeMethod_C4 / nCheap_ThreeMethod_C4)

######################
#####Simulation######
#####Definfe Models#####

model_names_3M <- c(
  "3M-biased-unique[.1]",
  "3M-biased-common[.1]",
  "3M-biased-unique[.4]",
  "3M-biased-common[.4]",
  "3M-mixed-unique[.1]",
  "3M-mixed-common[.1]",
  "3M-mixed-unique[.4]",
  "3M-mixed-common[.4]",
  "3M-valid-unique[.1]",
  "3M-valid-common[.1]",
  "3M-valid-unique[.4]",
  "3M-valid-common[.4]"
)

gen_models_3M <- c(
  ThreeMethod_popModel_33_1_1,
  ThreeMethod_popModel_33_1_5,
  ThreeMethod_popModel_33_4_1,
  ThreeMethod_popModel_33_4_5,
  ThreeMethod_popModel_35_1_1,
  ThreeMethod_popModel_35_1_5,
  ThreeMethod_popModel_35_4_1,
  ThreeMethod_popModel_35_4_5,
  ThreeMethod_popModel_55_1_1,
  ThreeMethod_popModel_55_1_5,
  ThreeMethod_popModel_55_4_1,
  ThreeMethod_popModel_55_4_5
)

analyze_models_3M <- rep(ThreeMethod_analyzeModel, 12)


three_method_models <-
  cbind(model_names_3M, gen_models_3M, analyze_models_3M)

model_names_2M <- c("2M-biased[.1]",
                    "2M-biased[.4]",
                    "2M-valid[.1]",
                    "2M-valid[.4]")

gen_models_2M <- c(
  TwoMethod_popModel_3_1,
  TwoMethod_popModel_3_4,
  TwoMethod_popModel_5_1,
  TwoMethod_popModel_5_4
)

analyze_models_2M <- rep(TwoMethod_analyzeModel, 4)

n_cc_TwoMethod <-
  c(n_cc_TwoMethod_A, n_cc_TwoMethod_B, n_cc_TwoMethod_C)
n_cc_TwoMethod <- as.data.frame(n_cc_TwoMethod)
rownames(n_cc_TwoMethod) <- c("A", "B", "C")
two_method_models <-
  cbind(model_names_2M, gen_models_2M, analyze_models_2M)

missing_pattern_ThreeMethod_A1 <-
  cbind(list(nCheap_ThreeMethod_A1),
        list(PercentMiss_ThreeMethod_A1))
missing_pattern_ThreeMethod_A2 <-
  cbind(list(nCheap_ThreeMethod_A2),
        list(PercentMiss_ThreeMethod_A2))
missing_pattern_ThreeMethod_A3 <-
  cbind(list(nCheap_ThreeMethod_A3),
        list(PercentMiss_ThreeMethod_A3))
missing_pattern_ThreeMethod_A4 <-
  cbind(list(nCheap_ThreeMethod_A4),
        list(PercentMiss_ThreeMethod_A4))
missing_pattern_ThreeMethod_B1 <-
  cbind(list(nCheap_ThreeMethod_B1),
        list(PercentMiss_ThreeMethod_B1))
missing_pattern_ThreeMethod_B2 <-
  cbind(list(nCheap_ThreeMethod_B2),
        list(PercentMiss_ThreeMethod_B2))
missing_pattern_ThreeMethod_B3 <-
  cbind(list(nCheap_ThreeMethod_B3),
        list(PercentMiss_ThreeMethod_B3))
missing_pattern_ThreeMethod_B4 <-
  cbind(list(nCheap_ThreeMethod_B4),
        list(PercentMiss_ThreeMethod_B4))
missing_pattern_ThreeMethod_C1 <-
  cbind(list(nCheap_ThreeMethod_C1),
        list(PercentMiss_ThreeMethod_C1))
missing_pattern_ThreeMethod_C2 <-
  cbind(list(nCheap_ThreeMethod_C2),
        list(PercentMiss_ThreeMethod_C2))
missing_pattern_ThreeMethod_C3 <-
  cbind(list(nCheap_ThreeMethod_C3),
        list(PercentMiss_ThreeMethod_C3))
missing_pattern_ThreeMethod_C4 <-
  cbind(list(nCheap_ThreeMethod_C4),
        list(PercentMiss_ThreeMethod_C4))


n_cc_ThreeMethod <-
  c(
    n_cc_ThreeMethod_A1,
    n_cc_ThreeMethod_A2,
    n_cc_ThreeMethod_A3,
    n_cc_ThreeMethod_A4,
    n_cc_ThreeMethod_B1,
    n_cc_ThreeMethod_B2,
    n_cc_ThreeMethod_B3,
    n_cc_ThreeMethod_B4,
    n_cc_ThreeMethod_C1,
    n_cc_ThreeMethod_C2,
    n_cc_ThreeMethod_C3,
    n_cc_ThreeMethod_C4
    
  )
n_cc_ThreeMethod <- as.data.frame(n_cc_ThreeMethod)
rownames(n_cc_ThreeMethod) <-
  c("A1", "A2", "A3", "A4","B1", "B2", "B3", "B4", "C1", "C2", "C3", "C4")

missing_patterns_ThreeMethod <-
  rbind(
    missing_pattern_ThreeMethod_A1,
    missing_pattern_ThreeMethod_A2,
    missing_pattern_ThreeMethod_A3,
    missing_pattern_ThreeMethod_A4,
    missing_pattern_ThreeMethod_B1,
    missing_pattern_ThreeMethod_B2,
    missing_pattern_ThreeMethod_B3,
    missing_pattern_ThreeMethod_B4,
    missing_pattern_ThreeMethod_C1,
    missing_pattern_ThreeMethod_C2,
    missing_pattern_ThreeMethod_C3,
    missing_pattern_ThreeMethod_C4
  )

colnames(missing_patterns_ThreeMethod) <- c("nCheap", "PercentMiss")
rownames(missing_patterns_ThreeMethod) <-
  c("A1", "A2", "A3", "A4", "B1", "B2", "B3","B4" , "C1", "C2", "C3","C4")

missing_pattern_TwoMethod_A <-
  cbind(list(nCheap_TwoMethod_A), list(PercentMiss_TwoMethod_A))
missing_pattern_TwoMethod_B <-
  cbind(list(nCheap_TwoMethod_B), list(PercentMiss_TwoMethod_B))
missing_pattern_TwoMethod_C <-
  cbind(list(nCheap_TwoMethod_C), list(PercentMiss_TwoMethod_C))

missing_patterns_TwoMethod <-
  rbind(
    missing_pattern_TwoMethod_A,
    missing_pattern_TwoMethod_B,
    missing_pattern_TwoMethod_C
  )


######Start Simulation#######
#### create directories ####
dir.create("Simulation")
dir.create("Simulation/PlannedMissingness")
dir.create("Simulation/PlannedMissingness/Data")
dir.create("Simulation/PlannedMissingness/SE")
dir.create("Simulation/PlannedMissingness/Simu")

for (i in 1:nrow(missing_patterns_ThreeMethod)) {
  dir.create(paste0(
    "Simulation/PlannedMissingness/Data/",
    rownames(missing_patterns_ThreeMethod)[i]
  ))
  dir.create(paste0(
    "Simulation/PlannedMissingness/SE/",
    rownames(missing_patterns_ThreeMethod)[i]
  ))
  dir.create(paste0(
    "Simulation/PlannedMissingness/Simu/",
    rownames(missing_patterns_ThreeMethod)[i]
  ))
}

for (i in 1:nrow(missing_patterns_TwoMethod)) {
  dir.create(paste0(
    "Simulation/PlannedMissingness/Data/",
    rownames(missing_patterns_TwoMethod)[i]
  ))
  dir.create(paste0(
    "Simulation/PlannedMissingness/SE/",
    rownames(missing_patterns_TwoMethod)[i]
  ))
  dir.create(paste0(
    "Simulation/PlannedMissingness/Simu/",
    rownames(missing_patterns_TwoMethod)[i]
  ))
}

dir.create("Simulation/CompleteCases")
dir.create("Simulation/CompleteCases/Data")
dir.create("Simulation/CompleteCases/SE")
dir.create("Simulation/CompleteCases/Simu")

for (i in 1:nrow(missing_patterns_ThreeMethod)) {
  dir.create(paste0(
    "Simulation/CompleteCases/Data/",
    rownames(missing_patterns_ThreeMethod)[i]
  ))
  dir.create(paste0(
    "Simulation/CompleteCases/SE/",
    rownames(missing_patterns_ThreeMethod)[i]
  ))
  dir.create(paste0(
    "Simulation/CompleteCases/Simu/",
    rownames(missing_patterns_ThreeMethod)[i]
  ))
}

for (i in 1:nrow(missing_patterns_TwoMethod)) {
  dir.create(paste0(
    "Simulation/CompleteCases/Data/",
    rownames(missing_patterns_TwoMethod)[i]
  ))
  dir.create(paste0(
    "Simulation/CompleteCases/SE/",
    rownames(missing_patterns_TwoMethod)[i]
  ))
  dir.create(paste0(
    "Simulation/CompleteCases/Simu/",
    rownames(missing_patterns_TwoMethod)[i]
  ))
}


##### define number of replications ####
number_of_replications = 1000

#####ThreeMethod Ncc######
for (row in 1:nrow(n_cc_ThreeMethod)) {
  nCC <- n_cc_ThreeMethod[row, "n_cc_ThreeMethod"]
  costRatio <- rownames(missing_patterns_ThreeMethod)[row]
  for (k in 1:nrow(three_method_models)) {
    analyze.model <- three_method_models[k, "analyze_models_3M"]
    gen.model <- three_method_models[k, "gen_models_3M"]
    mod <- three_method_models[k, "model_names_3M"]
    dataset <-
      sim(
        nRep = number_of_replications,
        model = analyze.model,
        n = nCC,
        generate = gen.model,
        lavaanfun = "lavaan",
        seed = 1234,
        dataOnly = TRUE,
        multicore = TRUE,
        auto.fix.first = FALSE,
        int.ov.free = TRUE,
        int.lv.free = FALSE,
        auto.fix.single = TRUE,
        auto.var = TRUE,
        auto.cov.lv.x = TRUE,
        auto.th = TRUE,
        auto.delta = TRUE,
        auto.cov.y = TRUE
      )
    out <- lapply(dataset, function(x) {
      lavaan(
        analyze.model,
        x,
        auto.fix.first = FALSE,
        int.ov.free = TRUE,
        int.lv.free = FALSE,
        auto.fix.single = TRUE,
        auto.var = TRUE,
        auto.cov.lv.x = TRUE,
        auto.th = TRUE,
        auto.delta = TRUE,
        auto.cov.y = TRUE
      )
    })
    ses <- c()
    for (j in 1:length(out)) {
      se <- out[[j]]@ParTable$se[which(out[[j]]@ParTable$op == "~")]
      ses[j] <- se
    }
    assign(paste0("Data_", mod, "_cc_", costRatio), dataset)
    assign(paste0("Simu_", mod, "_cc_", costRatio), out)
    assign(paste0("SE_", mod, "_cc_", costRatio), mean(ses))
    save(
      list = ls(pattern = "Data_"),
      file = paste0(
        "Simulation/CompleteCases/Data/",
        costRatio,
        "/Data_",
        mod,
        "_",
        costRatio,
        "_nCC.RData"
      )
    )
    save(
      list = ls(pattern = "Simu_"),
      file = paste0(
        "Simulation/CompleteCases/Simu/",
        costRatio,
        "/Simu_",
        mod,
        "_",
        costRatio,
        "_nCC.RData"
      )
    )
    save(
      list = ls(pattern = "SE_"),
      file = paste0(
        "Simulation/CompleteCases/SE/",
        costRatio,
        "/SE_list_",
        mod,
        "_",
        costRatio,
        "_nCC_.RData"
      )
    )
    rm(list = ls(pattern = "Simu_"))
    rm(list = ls(pattern = "Data_"))
    rm(list = ls(pattern = "SE_list"))
  }
}

#### ThreeMethod Planned Missingness ####
for (row in 1:nrow(missing_patterns_ThreeMethod)) {
  costRatio <- rownames(missing_patterns_ThreeMethod)[row]
  nCheap <- missing_patterns_ThreeMethod[row, "nCheap"][[1]]
  PercentMiss <-
    missing_patterns_ThreeMethod[row, "PercentMiss"][[1]]
  for (k in 1:nrow(three_method_models)) {
    analyze.model <- three_method_models[k, "analyze_models_3M"]
    gen.model <- three_method_models[k, "gen_models_3M"]
    mod <- three_method_models[k, "model_names_3M"]
    se_list <- c()
    for (i in 1:length(PercentMiss)) {
      miss.model <- miss(twoMethod = list(c(5:6), PercentMiss[i]))
      dataset <-
        sim(
          nRep = number_of_replications,
          model = analyze.model,
          n = nCheap[i],
          generate = gen.model,
          miss = miss.model,
          lavaanfun = "lavaan",
          seed = 1234,
          dataOnly = TRUE,
          multicore = TRUE,
          auto.fix.first = FALSE,
          int.ov.free = TRUE,
          int.lv.free = FALSE,
          auto.fix.single = TRUE,
          auto.var = TRUE,
          auto.cov.lv.x = TRUE,
          auto.th = TRUE,
          auto.delta = TRUE,
          auto.cov.y = TRUE
        )
      out <-
        lapply(dataset, function(x) {
          lavaan(
            model = analyze.model,
            x,
            missing = 'fiml',
            auto.fix.first = FALSE,
            int.ov.free = TRUE,
            int.lv.free = FALSE,
            auto.fix.single = TRUE,
            auto.var = TRUE,
            auto.cov.lv.x = TRUE,
            auto.th = TRUE,
            auto.delta = TRUE,
            auto.cov.y = TRUE
          )
        })
      ses <- c()
      for (j in 1:length(out)) {
        se <- out[[j]]@ParTable$se[which(out[[j]]@ParTable$op == "~")]
        ses[j] <- se
      }
      assign(paste0("Data_", mod, "_N_", nCheap[i], "_" , costRatio),
             dataset)
      assign(paste0("Simu_", mod, "_N_", nCheap[i], "_", costRatio), out)
      assign(paste0("SE_", mod, "_N_", nCheap[i], "_", costRatio),
             mean(ses))
      se_list[i] <- mean(ses, na.rm = TRUE)
    }
    assign(paste0("SE_list_", mod, "_", costRatio), se_list)
    save(
      list = ls(pattern = "Data_"),
      file = paste0(
        "Simulation/PlannedMissingness/Data/",
        costRatio,
        "/Data_",
        mod,
        "_",
        costRatio,
        ".RData"
      )
    )
    save(
      list = ls(pattern = "Simu_"),
      file = paste0(
        "Simulation/PlannedMissingness/Simu/",
        costRatio,
        "/Simu_",
        mod,
        "_",
        costRatio,
        ".RData"
      )
    )
    save(
      list = ls(pattern = "SE_list"),
      file = paste0(
        "Simulation/PlannedMissingness/SE/",
        costRatio,
        "/SE_list_",
        mod,
        "_",
        costRatio,
        ".RData"
      )
    )
    rm(list = ls(pattern = "Simu_"))
    rm(list = ls(pattern = "Data_"))
    rm(list = ls(pattern = "SE_list"))
  }
}

#####TwoMethod Ncc######
for (row in 1:nrow(n_cc_TwoMethod)) {
  nCC <- n_cc_TwoMethod[row, "n_cc_TwoMethod"]
  costRatio <- rownames(missing_patterns_TwoMethod)[row]
  for (k in 1:nrow(two_method_models)) {
    analyze.model <- two_method_models[k, "analyze_models_2M"]
    gen.model <- two_method_models[k, "gen_models_2M"]
    mod <- two_method_models[k, "model_names_2M"]
    dataset <-
      sim(
        nRep = number_of_replications,
        model = analyze.model,
        n = nCC,
        generate = gen.model,
        lavaanfun = "lavaan",
        seed = 1234,
        dataOnly = TRUE,
        multicore = TRUE,
        auto.fix.first = FALSE,
        int.ov.free = TRUE,
        int.lv.free = FALSE,
        auto.fix.single = TRUE,
        auto.var = TRUE,
        auto.cov.lv.x = TRUE,
        auto.th = TRUE,
        auto.delta = TRUE,
        auto.cov.y = TRUE
      )
    out <- lapply(dataset, function(x) {
      lavaan(
        analyze.model,
        x,
        auto.fix.first = FALSE,
        int.ov.free = TRUE,
        int.lv.free = FALSE,
        auto.fix.single = TRUE,
        auto.var = TRUE,
        auto.cov.lv.x = TRUE,
        auto.th = TRUE,
        auto.delta = TRUE,
        auto.cov.y = TRUE
      )
    })
    ses <- c()
    for (j in 1:length(out)) {
      se <- out[[j]]@ParTable$se[which(out[[j]]@ParTable$op == "~")]
      ses[j] <- se
    }
    assign(paste0("Data_", mod, "_cc_", costRatio), dataset)
    assign(paste0("Simu_", mod, "_cc_", costRatio), out)
    assign(paste0("SE_", mod, "_cc_", costRatio), mean(ses))
    save(
      list = ls(pattern = "Data_"),
      file = paste0(
        "Simulation/CompleteCases/Data/",
        costRatio,
        "/Data_",
        mod,
        "_",
        costRatio,
        "_nCC.RData"
      )
    )
    save(
      list = ls(pattern = "Simu_"),
      file = paste0(
        "Simulation/CompleteCases/Simu/",
        costRatio,
        "/Simu_",
        mod,
        "_",
        costRatio,
        "_nCC.RData"
      )
    )
    save(
      list = ls(pattern = "SE_"),
      file = paste0(
        "Simulation/CompleteCases/SE/",
        costRatio,
        "/SE_list_",
        mod,
        "_",
        costRatio,
        "nCC_.RData"
      )
    )
    rm(list = ls(pattern = "Simu_"))
    rm(list = ls(pattern = "Data_"))
    rm(list = ls(pattern = "SE_list"))
  }
}

#####TwoMethod Planned Missingness######
for (row in 1:nrow(missing_patterns_TwoMethod)) {
  costRatio <- rownames(missing_patterns_TwoMethod)[row]
  nCheap <- missing_patterns_TwoMethod[row, "nCheap"][[1]]
  PercentMiss <- missing_patterns_TwoMethod[row, "PercentMiss"][[1]]
  for (k in 1:nrow(two_method_models)) {
    analyze.model <- two_method_models[k, "analyze_models_2M"]
    gen.model <- two_method_models[k, "gen_models_2M"]
    mod <- two_method_models[k, "model_names_2M"]
    se_list <- c()
    for (i in 1:length(PercentMiss)) {
      miss.model <- miss(twoMethod = list(c(3:4), PercentMiss[i]))
      dataset <-
        sim(
          nRep = number_of_replications,
          model = analyze.model,
          n = nCheap[i],
          generate = gen.model,
          miss = miss.model,
          lavaanfun = "lavaan",
          seed = 1234,
          dataOnly = TRUE,
          multicore = TRUE,
          auto.fix.first = FALSE,
          int.ov.free = TRUE,
          int.lv.free = FALSE,
          auto.fix.single = TRUE,
          auto.var = TRUE,
          auto.cov.lv.x = TRUE,
          auto.th = TRUE,
          auto.delta = TRUE,
          auto.cov.y = TRUE
        )
      out <-
        lapply(dataset, function(x) {
          lavaan(
            model = analyze.model,
            x,
            missing = 'fiml',
            auto.fix.first = FALSE,
            int.ov.free = TRUE,
            int.lv.free = FALSE,
            auto.fix.single = TRUE,
            auto.var = TRUE,
            auto.cov.lv.x = TRUE,
            auto.th = TRUE,
            auto.delta = TRUE,
            auto.cov.y = TRUE
          )
        })
      ses <- c()
      for (j in 1:length(out)) {
        se <- out[[j]]@ParTable$se[which(out[[j]]@ParTable$op == "~")]
        ses[j] <- se
      }
      assign(paste0("Data_", mod, "_N_", nCheap[i], "_" , costRatio),
             dataset)
      assign(paste0("Simu_", mod, "_N_", nCheap[i], "_", costRatio), out)
      assign(paste0("SE_", mod, "_N_", nCheap[i], "_", costRatio),
             mean(ses))
      se_list[i] <- mean(ses, na.rm = TRUE)
    }
    assign(paste0("SE_list_", mod, "_", costRatio), se_list)
    save(
      list = ls(pattern = "Data_"),
      file = paste0(
        "Simulation/PlannedMissingness/Data/",
        costRatio,
        "/Data_",
        mod,
        "_",
        costRatio,
        ".RData"
      )
    )
    save(
      list = ls(pattern = "Simu_"),
      file = paste0(
        "Simulation/PlannedMissingness/Simu/",
        costRatio,
        "/Simu_",
        mod,
        "_",
        costRatio,
        ".RData"
      )
    )
    save(
      list = ls(pattern = "SE_list"),
      file = paste0(
        "Simulation/PlannedMissingness/SE/",
        costRatio,
        "/SE_list_",
        mod,
        "_",
        costRatio,
        ".RData"
      )
    )
    rm(list = ls(pattern = "Simu_"))
    rm(list = ls(pattern = "Data_"))
    rm(list = ls(pattern = "SE_list"))
  }
}
