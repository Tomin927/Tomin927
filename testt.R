
## -----------------------------------------------------------------------------
## Import Dataset
## -----------------------------------------------------------------------------
library(readr)
library(stringr)

# Disturbance-Storm-Time Index (Dst) Data
Dst_df <- read_csv("C:/Users/tommy/Desktop/IC Stat/Research/archive/labels.csv")

# Split train sets
Dst_raw_a <- subset(Dst_df, period == 'train_a')[,-1] # 1201 days
Dst_raw_b <- subset(Dst_df, period == 'train_b')[,-1] # 2191 days
Dst_raw_c <- subset(Dst_df, period == 'train_c')[,-1] # 2436 days

# Modify the dataset into dyadic one
Dst_dyadic_a <- Dst_raw_a[1:2**14,]
Dst_dyadic_b <- Dst_raw_b[1:2**15,]
Dst_dyadic_c <- Dst_raw_c[1:2**15,]

# Real-Time Solar-Wind (RTSW) Data
solar_wind_df <- read_csv("C:/Users/tommy/Desktop/IC Stat/Research/archive/solar_wind.csv")

# Extract potential covariates: 
#   bt          - Interplanetary-magnetic-field component magnitude (nT)
#   bz_gsm      - Interplanetary-magnetic-field Z-component in GSM coordinate (nT)
#   density     - Solar wind proton density (N/cm^3)
#   speed       - Solar wind bulk speed (km/s)
#   temperature - Solar wind ion temperature (Kelvin)
RTSW_raw <- solar_wind_df[,c('period', 'timedelta', 'bt', 'bz_gsm', 
                             'density', 'speed', 'temperature')]

# Match the time index with Dst data
RTSW_matched <- subset(RTSW_raw, timedelta %in% Dst_df$timedelta)

# Split train sets
RTSW_raw_a <- subset(RTSW_matched, period == 'train_a')[,-1] # 1201 days
RTSW_raw_b <- subset(RTSW_matched, period == 'train_b')[,-1] # 2191 days
RTSW_raw_c <- subset(RTSW_matched, period == 'train_c')[,-1] # 2436 days

# Modify the dataset into dyadic one
RTSW_dyadic_a <- RTSW_raw_a[1:2**14,]
RTSW_dyadic_b <- RTSW_raw_b[1:2**15,]
RTSW_dyadic_c <- RTSW_raw_c[1:2**15,]

# Sun Spot Data
ssn_raw <- read_csv("C:/Users/tommy/Desktop/IC Stat/Research/archive/sunspots.csv")

# Split train sets
ssn_raw_a <- subset(ssn_raw, period == 'train_a')[,-1] 
ssn_raw_b <- subset(ssn_raw, period == 'train_b')[,-1] 
ssn_raw_c <- subset(ssn_raw, period == 'train_c')[,-1] 

ssn_raw_a$day <- as.integer(str_split_fixed(ssn_raw_a$timedelta, " ", 2)[,1])
ssn_raw_b$day <- as.integer(str_split_fixed(ssn_raw_b$timedelta, " ", 2)[,1])
ssn_raw_c$day <- as.integer(str_split_fixed(ssn_raw_c$timedelta, " ", 2)[,1])

## -----------------------------------------------------------------------------
## Overview of the Dataset
## -----------------------------------------------------------------------------

# Plot of the Dst time series data with sun spot number
par(mar = c(5, 4, 2, 4) + 0.3)  
ts.plot(ts(-Dst_raw_a$dst, start=0, frequency=24), 
        ts(-Dst_dyadic_a$dst, start=0, frequency=24), 
        xlab='Time (Day)', ylab='Dst (nT) below 0',
        main='Negated Disturbance-Storm-Time (Dst) Index',
        gpars = list(lty=c(3,1), col=c(8,1)))
abline(v=2**14/24, col="red")
legend("topleft", legend=c('observed (dyadic) value', 
                           'unobserved (masked) value', 
                           'Sun Spot Number'), 
       lty=c(1,3,2), col=c(1,8,1), cex=.75, bty="n")

par(new = TRUE)
plot((1:nrow(Dst_raw_a))/24, rep(ssn_a, each=24), type = 'l', 
     lty=2, axes = FALSE, bty = "n", xlab = "", ylab = "")
axis(side=4, at = as.integer(ssn_a/20)*20)

# Plot RTSW time series data
oldpar <- par(mfrow=c(2,2))
ts.plot(ts(RTSW_raw_a$bt, start=0, frequency=24), 
        ts(RTSW_raw_a$bz_gsm, start=0, frequency=24),
        ts(RTSW_dyadic_a$bt, start=0, frequency=24), 
        ts(RTSW_dyadic_a$bz_gsm, start=0, frequency=24),
        xlab='Time (Day)', ylab='Bt & Bz GSM (nT)', sub="(a)",
        main='Interplanetary-Magnetic-Field (IMF) Index',
        gpars = list(lty=c(3,3,1,1), col=c(8,5,1,4)))
abline(v=2**14/24, col="red")
legend("bottomleft", legend=c('IMF magnitude', 'IMF z-component (GSM)'), 
       lty=c(1,1), col=c(1,4,1,4), cex=.5, bty="n")
legend("topleft", legend="Observed", cex=.75, bty="n")
legend("topright", legend="Masked", cex=.75, bty="n")

ts.plot(ts(RTSW_raw_a$density, start=0, frequency=24), 
        ts(RTSW_dyadic_a$density, start=0, frequency=24), 
        xlab='Time (Day)', ylab='Density (N/cm^3)', sub="(b)",
        main='Solar Wind Proton Density',
        gpars = list(lty=c(3,1), col=c(8,1)))
abline(v=2**14/24, col="red")
legend("topleft", legend="Observed", cex=.75, bty="n")
legend("topright", legend="Masked", cex=.75, bty="n")

ts.plot(ts(RTSW_raw_a$speed, start=0, frequency=24), 
        ts(RTSW_dyadic_a$speed, start=0, frequency=24), 
        xlab='Time (Day)', ylab='Speed (km/s)', sub="(c)",
        main='Solar Wind Bulk Speed',
        gpars = list(lty=c(3,1), col=c(8,1)))
abline(v=2**14/24, col="red")
legend("topleft", legend="Observed", cex=.75, bty="n")
legend("topright", legend="Masked", cex=.75, bty="n")

ts.plot(ts(RTSW_raw_a$temperature, start=0, frequency=24), 
        ts(RTSW_dyadic_a$temperature, start=0, frequency=24), 
        xlab='Time (Day)', ylab='Temperature (Kelvin)', sub="(d)",
        main='Solar Wind Ion Temperature',
        gpars = list(lty=c(3,1), col=c(8,1)))
abline(v=2**14/24, col="red")
legend("topleft", legend="Observed", cex=.75, bty="n")
legend("topright", legend="Masked", cex=.75, bty="n")
par(oldpar)

## ------------- Additional plots of train set b and c -------------------------

# Dst plot
oldpar <- par(mfrow=c(1,2))
par(mar = c(5, 4, 2, 4) + 0.3)  
ts.plot(ts(-Dst_raw_b$dst, start=0, frequency=24), 
        ts(-Dst_dyadic_b$dst, start=0, frequency=24), 
        xlab='Time (Day)', ylab='Dst (nT) below 0', sub="(a)",
        main='Negated Disturbance-Storm-Time (Dst) Index - b',
        gpars = list(lty=c(3,1), col=c(8,1)))
abline(v=2**15/24, col="red")
legend("topleft", legend="Observed", cex=.75, bty="n")
legend("topright", legend="Masked", cex=.75, bty="n")

par(new = TRUE)
plot((1:nrow(Dst_raw_b))/24, rep(ssn_b, each=24), type = 'l', 
     lty=2, axes = FALSE, bty = "n", xlab = "", ylab = "")
axis(side=4, at = as.integer(ssn_b/20)*20)

par(mar = c(5, 4, 2, 4) + 0.3)  
ts.plot(ts(-Dst_raw_c$dst, start=0, frequency=24), 
        ts(-Dst_dyadic_c$dst, start=0, frequency=24), 
        xlab='Time (Day)', ylab='Dst (nT) below 0', sub="(b)",
        main='Negated Disturbance-Storm-Time (Dst) Index - c',
        gpars = list(lty=c(3,1), col=c(8,1)))
abline(v=2**15/24, col="red")
legend("topleft", legend="Observed", cex=.75, bty="n")
legend("topright", legend="Masked", cex=.75, bty="n")

par(new = TRUE)
plot((1:nrow(Dst_raw_c))/24, rep(ssn_c, each=24), type = 'l', 
     lty=2, axes = FALSE, bty = "n", xlab = "", ylab = "")
axis(side=4, at = as.integer(ssn_c/20)*20)
par(oldpar)

# RTSW plot
oldpar <- par(mfrow=c(2,2))
ts.plot(ts(RTSW_raw_b$bt, start=0, frequency=24), 
        ts(RTSW_raw_b$bz_gsm, start=0, frequency=24),
        ts(RTSW_dyadic_b$bt, start=0, frequency=24), 
        ts(RTSW_dyadic_b$bz_gsm, start=0, frequency=24),
        xlab='Time (Day)', ylab='Bt & Bz GSM (nT)', sub="(a)",
        main='Interplanetary-Magnetic-Field (IMF) Index - b',
        gpars = list(lty=c(3,3,1,1), col=c(8,5,1,4)))
abline(v=2**15/24, col="red")
legend("topleft", legend="Observed", cex=.75, bty="n")
legend("topright", legend="Masked", cex=.75, bty="n")

ts.plot(ts(RTSW_raw_b$density, start=0, frequency=24), 
        ts(RTSW_dyadic_b$density, start=0, frequency=24), 
        xlab='Time (Day)', ylab='Density (N/cm^3)', sub="(b)",
        main='Solar Wind Proton Density - b',
        gpars = list(lty=c(3,1), col=c(8,1)))
abline(v=2**15/24, col="red")
legend("topleft", legend="Observed", cex=.75, bty="n")
legend("topright", legend="Masked", cex=.75, bty="n")

ts.plot(ts(RTSW_raw_b$speed, start=0, frequency=24), 
        ts(RTSW_dyadic_b$speed, start=0, frequency=24), 
        xlab='Time (Day)', ylab='Speed (km/s)', sub="(c)",
        main='Solar Wind Bulk Speed - b',
        gpars = list(lty=c(3,1), col=c(8,1)))
abline(v=2**15/24, col="red")
legend("topleft", legend="Observed", cex=.75, bty="n")
legend("topright", legend="Masked", cex=.75, bty="n")

ts.plot(ts(RTSW_raw_b$temperature, start=0, frequency=24), 
        ts(RTSW_dyadic_b$temperature, start=0, frequency=24), 
        xlab='Time (Day)', ylab='Temperature (Kelvin)', sub="(d)",
        main='Solar Wind Ion Temperature - b',
        gpars = list(lty=c(3,1), col=c(8,1)))
abline(v=2**15/24, col="red")
legend("topleft", legend="Observed", cex=.75, bty="n")
legend("topright", legend="Masked", cex=.75, bty="n")
par(oldpar)

oldpar <- par(mfrow=c(2,2))
ts.plot(ts(RTSW_raw_c$bt, start=0, frequency=24), 
        ts(RTSW_raw_c$bz_gsm, start=0, frequency=24),
        ts(RTSW_dyadic_c$bt, start=0, frequency=24), 
        ts(RTSW_dyadic_c$bz_gsm, start=0, frequency=24),
        xlab='Time (Day)', ylab='Bt & Bz GSM (nT)', sub="(a)",
        main='Interplanetary-Magnetic-Field (IMF) Index - c',
        gpars = list(lty=c(3,3,1,1), col=c(8,5,1,4)))
abline(v=2**15/24, col="red")
legend("topleft", legend="Observed", cex=.75, bty="n")
legend("topright", legend="Masked", cex=.75, bty="n")

ts.plot(ts(RTSW_raw_c$density, start=0, frequency=24), 
        ts(RTSW_dyadic_c$density, start=0, frequency=24), 
        xlab='Time (Day)', ylab='Density (N/cm^3)', sub="(b)",
        main='Solar Wind Proton Density - c',
        gpars = list(lty=c(3,1), col=c(8,1)))
abline(v=2**15/24, col="red")
legend("topleft", legend="Observed", cex=.75, bty="n")
legend("topright", legend="Masked", cex=.75, bty="n")

ts.plot(ts(RTSW_raw_c$speed, start=0, frequency=24), 
        ts(RTSW_dyadic_c$speed, start=0, frequency=24), 
        xlab='Time (Day)', ylab='Speed (km/s)', sub="(c)",
        main='Solar Wind Bulk Speed - c',
        gpars = list(lty=c(3,1), col=c(8,1)))
abline(v=2**15/24, col="red")
legend("topleft", legend="Observed", cex=.75, bty="n")
legend("topright", legend="Masked", cex=.75, bty="n")

ts.plot(ts(RTSW_raw_c$temperature, start=0, frequency=24), 
        ts(RTSW_dyadic_c$temperature, start=0, frequency=24), 
        xlab='Time (Day)', ylab='Temperature (Kelvin)', sub="(d)",
        main='Solar Wind Ion Temperature - c',
        gpars = list(lty=c(3,1), col=c(8,1)))
abline(v=2**15/24, col="red")
legend("topleft", legend="Observed", cex=.75, bty="n")
legend("topright", legend="Masked", cex=.75, bty="n")
par(oldpar)

## -----------------------------------------------------------------------------
## Denoising Process (Dst Dataset a)
## -----------------------------------------------------------------------------
library(wavethresh)

n <- 2**14 # number of observations in Dst data a

## ----------------------- Universal Threshold ---------------------------------

# Perform a DWT on negated Dst data
dst_wd <- wd(-Dst_dyadic_a$dst)

# Compute the universal threshold using the finest-level information only
FineCoefs <- accessD(dst_wd, lev=dst_wd$nlevels-1)
sigma <- mad(FineCoefs)
utDJ <- sigma*sqrt(2*log(n))

# Apply the univesal threshold & Reconstruct an estimate
dst_wduT <- threshold(dst_wd, policy="manual", value=utDJ)
dst_wruT <- wr(dst_wduT)

# Plot of DWT on Dst before and after denoising for comparison
oldpar <- par(mfrow=c(2,2))
plot(dst_wd, main="DWT on Noisy Dst", sub="(a)",
     scaling="by.level",
     xlab="Translate")
plot(dst_wduT, main="Universal Shrinkage", sub="(b)",
     scaling="by.level",
     xlab="Translate")

# Plot of the estimate
plot((1:200)/24, -Dst_dyadic_a$dst[1:200], type="l", main="Original Data (Partial)", 
     sub="(c)", xlab="Time (Day)", ylab="-Dst (nT)")
plot((1:200)/24, dst_wruT[1:200], type="l", main="Univ. Estimate (Partial)", 
     sub="(d)", xlab="Time (Day)", ylab="-Dst (nT)")
par(oldpar)

## -------------------- Cross-validated & FDR Threshold ------------------------

# Apply the cross-validated threshold
dst_wdcvT <- threshold(dst_wd, policy="cv", dev=madmad)
dst_wrcvT <- wr(dst_wdcvT)

# Apply the FDR threshold
dst_wdfdrT <- threshold(dst_wd, policy="fdr")
dst_wrfdrT <- wr(dst_wdfdrT)

# Plot of DWT for comparison
oldpar <- par(mfrow=c(2,2))
plot(dst_wdcvT, main="Cross-validated Shrinkage", sub="(a)",
     scaling="by.level",
     xlab="Translate")
plot(dst_wdfdrT, main="FDR Shrinkage", sub="(b)",
     scaling="by.level",
     xlab="Translate")

# Plot of the estimate
plot((1:200)/24, dst_wrcvT[1:200], type="l", sub="(c)", 
     main="Cross-val. Estimate (Partial)", 
     xlab="Time (Day)", ylab="-Dst (nT)")
plot((1:200)/24, dst_wrfdrT[1:200], type="l", sub="(d)",
     main="FDR Estimate (Partial)", 
     xlab="Time (Day)", ylab="-Dst (nT)")
par(oldpar)

## ----------------------- Empirical Bayes Threshold ---------------------------
##                           (Mixture of Gaussians)
##                   (Mixture of point mass and heavy tail)
library("EbayesThresh")

# Mixture of Gaussians threshold
dst_wdbT <- threshold(dst_wd, policy="BayesThresh")
dst_wrbT <- wr(dst_wdbT)

# Mixture of point mass and heavy tail threshold
dst_wdebT <- ebayesthresh.wavelet(dst_wd)
dst_wrebT <- wr(dst_wdebT)

# Plot of DWT for comparison
oldpar <- par(mfrow=c(2,2))
plot(dst_wdbT, main="Mixture of Gaussians Shrinkage", sub="(a)",
     scaling="by.level",
     xlab="Translate")
plot(dst_wdebT, main="Mixture of point mass and heavy tail Shrinkage", sub="(b)",
     scaling="by.level",
     xlab="Translate")

# Plot of the estimate
plot((1:200)/24, dst_wrbT[1:200], type="l", sub="(c)", 
     main="Mixture of Gaussians Estimate (Partial)", 
     xlab="Time (Day)", ylab="-Dst (nT)")
plot((1:200)/24, dst_wrebT[1:200], type="l", sub="(d)",
     main="Mixture of point mass and heavy tail Estimate (Partial)", 
     xlab="Time (Day)", ylab="-Dst (nT)")
par(oldpar)

## ----------------- Translation-invariant (TI) Threshold ----------------------
##                     (Average-Basis & Basis selection)

# Compute the NDWT (packet-ordered) on Dst data
dst_wst <- wst(-Dst_dyadic_a$dst) 

# Apply the univesal threshold
FineWSTCoefs <- accessD(dst_wst, lev=dst_wst$nlevels-1)
sigmaWST <- mad(FineWSTCoefs)
utWSTDJ <- sigmaWST*sqrt(2*log(n))

dst_wstuT <- threshold(dst_wst, policy="manual", value=utWSTDJ)

# Reconstruct an estimate (Average-Basis)
dst_ABtiT <- AvBasis(dst_wstuT) 

# Basis selection
# Create space for recording performance for each shifted basis. 
#   there is one shift for each element of Dst data
rss <- rep(0, n)

# for each shift, i, first compute the node vector for that shift 
#   (which defines the basis). 
#   then invert 'dst_wstuT' using the packets defined by the node vector. 
#   form and store the measure of performance
for(i in 1:n) {
        thenv <- numtonv(i-1, dst_wstuT$nlevels)
        therecon <- InvBasis(dst_wstuT, nv=thenv)
        rss[i] <- sqrt(sum((therecon + Dst_dyadic_a$dst)^2))
}

# report performance for the standard wavelet basis, and the best one.
cat("Standard wavelet basis RSS is ", rss[1], "\n")
cat("Best wavelet basis RSS is ", min(rss), "\n")

# plot of the performance on each basis
plot(1:512, rss[1:512], type="l",
     xlab="Basis Index (Partial)",
     ylab="RSS")

# Reconstruct an estimate (Best-Basis)
dst_BBtiT <- InvBasis(dst_wstuT, nv=numtonv(which.min(rss), dst_wstuT$nlevels))

# Plot of NDWT for comparison
oldpar <- par(mfrow=c(2,2))
plot(wst(dst_ABtiT), main="Translation-invariant Shrinkage (Average-Basis)", 
     sub="(a)", first.level = 10, scaling="by.level")
plot(wst(dst_BBtiT), main="Translation-invariant Shrinkage (Best-Basis)", 
     sub="(b)", first.level = 10, scaling="by.level")

# Plot of the estimate
plot((1:200)/24, dst_ABtiT[1:200], type="l", sub="(c)", 
     main="TI Estimate (Average-Basis) (Partial)", 
     xlab="Time (Day)", ylab="-Dst (nT)")
plot((1:200)/24, dst_BBtiT[1:200], type="l", sub="(d)",
     main="TI Estimate (Best-Basis) (Partial)", 
     xlab="Time (Day)", ylab="-Dst (nT)")
par(oldpar)

## ------------------------- Data-driven Haar-Fisz -----------------------------
library(DDHFm)

# Perform the DDHFT
dst_ddhft <- ddhft.np.2(ts(-Dst_dyadic_a$dst, start=0, frequency=24))

# Appliy the TI Shrinkage with universal threshold
dst_ddhft_wst <- wst(dst_ddhft$hft) 

FineWSTCoefs_ddhft <- accessD(dst_ddhft_wst, lev=dst_ddhft_wst$nlevels-1)
sigmaWST_ddhft <- mad(FineWSTCoefs_ddhft)
utWSTDJ_ddhft <- sigmaWST_ddhft*sqrt(2*log(n))

dst_ddhft_wstuT <- threshold(dst_ddhft_wst, policy="manual", value=utWSTDJ_ddhft)
dst_ddhft_ABtiT <- AvBasis(dst_ddhft_wstuT) 

# Perform the inverse DDHFT
dst_ddhft_ABtiT_inv <- dst_ddhft
dst_ddhft_ABtiT_inv[1] <- list(dst_ddhft_ABtiT)
dst_ddhft_ABtiT_inv <- ddhft.np.inv(dst_ddhft_ABtiT_inv)

# Plot of the mean-variance relationship
oldpar <- par(mfrow=c(3,2))
plot(dst_ddhft$mu, sqrt(dst_ddhft$sigma2), sub="(a)",
     xlab="mu (nT)", ylab="sqrt(sigma) (nT)",
     main="Mean-Variance Relationship")

plot(dst_ddhft$mu, dst_ddhft$sigma, type='l', sub="(b)",
     xlab="mu (nT)", ylab="sqrt(h(mu)) (nT)",
     main="Empirical Estimate of the Mean-Variance Relationship")

# Plot of the stabilized sequences and its residuals with denoising
ts.plot(ts(dst_ddhft$hft, start=0, frequency=24), 
        xlab='Time (Day)', ylab='DDHFT of Dst', sub="(c)",
        main='Negated Variance Stabilized DDHFT Dst Data')

ts.plot(ts(dst_ddhft$hft-dst_ddhft_ABtiT, start=0, frequency=24), 
        xlab='Time (Day)', ylab="", sub="(d)",
        main='Residuals of DDHFT Dst Data after Denoising')

# Plot of the inversed DDHFT stabilized sequences and its residuals with denoising
plot((1:200)/24, dst_ddhft_ABtiT_inv[1:200], type='l',
     xlab='Time (Day)', ylab='Dst (nT) below 0', sub="(e)",
     main='Negated Denoised Variance Stabilized Dst Data (Partial)')

ts.plot(ts(-Dst_dyadic_a$dst-dst_ddhft_ABtiT_inv, start=0, frequency=24), 
        xlab='Time (Day)', ylab="", sub="(f)",
        main='Residuals of Original Data against DDHFT Denoising')
par(oldpar)

## -----------------------------------------------------------------------------
## LSW Analysis of Dst Data
## -----------------------------------------------------------------------------
library(forecast)

# Compute corrected smoothed wavelet periodgram
ddst <- diff(c(dst_ddhft_ABtiT_inv[2], dst_ddhft_ABtiT_inv))
ddst_spec <- ewspec(ddst, 
               smooth.levels=4:(log2(n)-1),
               smooth.policy="universal", 
               smooth.transform=log,
               smooth.inverse=exp)
raw_ews <- ddst_spec$WavPer
corrected_ews <- ddst_spec$S

# Plot of the differenced Dst time series data
oldpar <- par(mfrow=c(2,2))
ts.plot(ts(-Dst_dyadic_a$dst, start=0, frequency=24), sub="(a)",
        xlab='Time (Day)', ylab='Original Dst Data', 
        main='The Original Dst Data')
ts.plot(ts(dst_ddhft_ABtiT_inv, start=0, frequency=24), sub="(b)",
        xlab='Time (Day)', ylab='', 
        main='Negated Denoised Variance Stabilized Dst Data')
ts.plot(ts(diff(-Dst_dyadic_a$dst), start=0, frequency=24), sub="(c)",
        xlab='Time (Day)', ylab='Differenced Dst value with lag 1', 
        main='The Original Dst Data Difference')
ts.plot(ts(ddst, start=0, frequency=24), sub="(d)",
        xlab='Time (Day)', ylab='', 
        main='Variance-stabilized TI Estimate Difference')
par(oldpar)

# Plot of the EWS estimate
oldpar <- par(mfrow=c(1,2))
plot(raw_ews, scaling="by.level",  sub="(a)",
     xlab="Time (Days)", ylab="Scale",
     main="Rwe Smoothed Wavelet Periodogram",
     xlabvals=24*c(0,170,340,510,680), xlabchars=c('0','170','340','510','680'))
plot(corrected_ews, scaling="by.level",  sub="(b)",
     xlab="Time (Days)", ylab="Scale",
     main="Corrected Smoothed Wavelet Periodogram",
     xlabvals=24*c(0,170,340,510,680), xlabchars=c('0','170','340','510','680'))
par(oldpar)

# Plot of the covariate for comparison
oldpar <- par(mfrow=c(2,2))
par(mar = c(5, 4, 2, 4) + 0.3)  
plot(accessD(ddst_spec, level=log2(n)-1), type='l', xaxt = 'n', 
     xlab='Translate', ylab='EWS at finest scale')
par(new = TRUE)
plot(ifelse(RTSW_dyadic_a$bz_gsm < -20, 1, 0), type = 'l', sub = "(a)",
     lty=2, axes = FALSE, bty = "n", xlab = "", ylab = "", col=2)
legend("topleft", legend="bz_gsm < -20 nT", cex=.75, bty="n")
axis(side=4, at = 0.5, labels=c('Bz'))

par(mar = c(5, 4, 2, 4) + 0.3)  
plot(accessD(ddst_spec, level=log2(n)-1), type='l', xaxt = 'n', 
     xlab='Translate', ylab='EWS at finest scale')
par(new = TRUE)
plot(ifelse(RTSW_dyadic_a$density > 50, 1, 0), type = 'l', sub = "(b)",
     lty=2, axes = FALSE, bty = "n", xlab = "", ylab = "", col=3)
legend("topleft", legend="density > 50 N/cm^3", cex=.75, bty="n")
axis(side=4, at = 0.5, labels=c('Density'))

par(mar = c(5, 4, 2, 4) + 0.3)  
plot(accessD(ddst_spec, level=log2(n)-1), type='l', xaxt = 'n', 
     xlab='Translate', ylab='EWS at finest scale')
par(new = TRUE)
plot(ifelse(RTSW_dyadic_a$speed > 750, 1, 0), type = 'l', sub = "(c)",
     lty=2, axes = FALSE, bty = "n", xlab = "", ylab = "", col=4)
legend("topleft", legend="speed > 750 km/s", cex=.75, bty="n")
axis(side=4, at = 0.5, labels=c('Speed'))

par(mar = c(5, 4, 2, 4) + 0.3)  
plot(accessD(ddst_spec, level=log2(n)-1), type='l', xaxt = 'n', 
     xlab='Translate', ylab='EWS at finest scale')
par(new = TRUE)
plot(ifelse(RTSW_dyadic_a$temperature > 800000, 1, 0), type = 'l', sub = "(d)",
     lty=2, axes = FALSE, bty = "n", xlab = "", ylab = "", col=6)
legend("topleft", legend="temperature > 8e5 Kelvin", cex=.75, bty="n")
axis(side=4, at = 0.5, labels=c('Temp'))
par(oldpar)

# ---------------------- Shortern Forecasting with LSW -------------------------
library(forecastLSW)

nsteps <- 24*3 # 3-day forecasting
rng <- (n - nsteps+1):n

dst_obs <- dst_ddhft_ABtiT_inv[1:(n - nsteps)]
dst_mean <- mean(dst_obs)
dst_obs <- dst_obs - dst_mean

ppred <- forecastlpacf(-dst_obs, h=nsteps, forecast.type='extend')$mean - dst_mean
ppred_rescaled <- ppred/sqrt(sd(ppred))

yr <- range(c(Dst_raw_a$dst[rng], ppred_rescaled))
ts.plot(ts(Dst_raw_a$dst[rng], start=0, frequency=24),
        ylim=yr, xlab='Time (Day)', ylab='Dst (nT)', 
        main='Forecasting of Dst in 3 Days')
lines(1:(nsteps)/24, ppred_rescaled, lty=2)
points(1:(nsteps)/24, ppred_rescaled, lty=2)
legend("bottomleft", legend=c('Original Dst Data', 'Forcasting Value'), 
       lty=c(1,2), cex=.5)

detach("package:forecastLSW", unload=TRUE)
detach("package:lpacf", unload=TRUE)
detach("package:locits", unload=TRUE)
detach("package:igraph", unload=TRUE)
detach("package:parallel", unload=TRUE)

## -----------------------------------------------------------------------------
## EVT Analysis of Dst Data
## -----------------------------------------------------------------------------
library(evd)
library(ismev)
library(evgam)
library(LBI)

# Perform the DDHFT and denoising on Dst and RTSW data
ddhft_denoising <- function(noisy_ts, n=2**14) {
        # Perform the DDHFT
        x_ddhft <- dst_ddhft <- ddhft.np.2(ts(noisy_ts, start=0, frequency=24))
        
        # Appliy the TI wavelet transformation
        x_ddhft_wst <- wst(x_ddhft$hft) 
        
        FineWSTCoefs_ddhft <- accessD(x_ddhft_wst, lev=x_ddhft_wst$nlevels-1)
        sigmaWST_ddhft <- mad(FineWSTCoefs_ddhft)
        utWSTDJ_ddhft <- sigmaWST_ddhft*sqrt(2*log(n))
        
        x_ddhft_wsttiT <- threshold(x_ddhft_wst, policy="manual", value=utWSTDJ_ddhft)
        x_ddhft_ABtiT <- AvBasis(x_ddhft_wsttiT) 
        
        # Perform the inverse DDHFT
        x_ddhft_ABtiT_inv <- x_ddhft
        x_ddhft_ABtiT_inv[1] <- list(x_ddhft_ABtiT)
        x_ddhft_ABtiT_inv <- ddhft.np.inv(x_ddhft_ABtiT_inv)
}

dst_ddhft_a <- ddhft_denoising(ts(-Dst_dyadic_a$dst, start=0, frequency=24))
dst_ddhft_b <- ddhft_denoising(ts(-Dst_dyadic_b$dst, start=0, frequency=24), n=2**15)
dst_ddhft_c <- ddhft_denoising(ts(-Dst_dyadic_c$dst, start=0, frequency=24), n=2**15)

# Prepare the denoised dataframe with RTSW data (Covariate)
dst_evt_a <- data.frame(timedelta = Dst_dyadic_a$timedelta, 
                        day = as.integer(str_split_fixed(Dst_dyadic_a$timedelta, " ", 2)[,1]),
                        dst = dst_ddhft_a)
dst_evt_b <- data.frame(timedelta = Dst_dyadic_b$timedelta, 
                        day = as.integer(str_split_fixed(Dst_dyadic_b$timedelta, " ", 2)[,1]),
                        dst = dst_ddhft_b)
dst_evt_c <- data.frame(timedelta = Dst_dyadic_c$timedelta, 
                        day = as.integer(str_split_fixed(Dst_dyadic_c$timedelta, " ", 2)[,1]),
                        dst = dst_ddhft_c)

dst_evt_a <- na.omit(cbind(dst_evt_a, RTSW_dyadic_a[, 3:6]))
dst_evt_b <- na.omit(cbind(dst_evt_b, RTSW_dyadic_b[, 3:6]))
dst_evt_c <- na.omit(cbind(dst_evt_c, RTSW_dyadic_c[, 3:6]))

ssn_a <- c()
for (i in 1:nrow(ssn_raw_a)) {
    if (i < nrow(ssn_raw_a)) {
        ssn_a <- c(ssn_a, rep(ssn_raw_a$smoothed_ssn[i], 
                              ssn_raw_a$day[i+1]-ssn_raw_a$day[i]))
    }
    else {
        ssn_a <- c(ssn_a, rep(ssn_raw_a$smoothed_ssn[i], 
                              nrow(Dst_raw_a)/24-ssn_raw_a$day[i]))
    }
}

ssn_b <- c()
for (i in 1:nrow(ssn_raw_b)) {
    if (i < nrow(ssn_raw_b)) {
        ssn_b <- c(ssn_b, rep(ssn_raw_b$smoothed_ssn[i], 
                              ssn_raw_b$day[i+1]-ssn_raw_b$day[i]))
    }
    else {
        ssn_b <- c(ssn_b, rep(ssn_raw_b$smoothed_ssn[i], 
                              nrow(Dst_raw_b)/24-ssn_raw_b$day[i]))
    }
}

ssn_c <- c()
for (i in 1:nrow(ssn_raw_c)) {
    if (i < nrow(ssn_raw_c)) {
        ssn_c <- c(ssn_c, rep(ssn_raw_c$smoothed_ssn[i], 
                              ssn_raw_c$day[i+1]-ssn_raw_c$day[i]))
    }
    else {
        ssn_c <- c(ssn_c, rep(ssn_raw_c$smoothed_ssn[i], 
                              nrow(Dst_raw_c)/24-ssn_raw_c$day[i]))
    }
}

dst_evt_a$ssn <- ssn_a[dst_evt_a$day+1]
dst_evt_b$ssn <- ssn_b[dst_evt_b$day+1]
dst_evt_c$ssn <- ssn_c[dst_evt_c$day+1]

# ---------------------------------- GEV Models --------------------------------

# Prepare and conjoin daily maximum data with the RTSW data (Covariates)
dst_gev_a <- aggregate(dst ~ day, dst_evt_a, max)
dst_gev_b <- aggregate(dst ~ day, dst_evt_b, max)
dst_gev_c <- aggregate(dst ~ day, dst_evt_c, max)

dst_gev_a <- cbind(dst_gev_a, dst_evt_a[dst_evt_a$dst %in% dst_gev_a$dst, 4:7])
dst_gev_b <- cbind(dst_gev_b, dst_evt_b[dst_evt_b$dst %in% dst_gev_b$dst, 4:7])
dst_gev_c <- cbind(dst_gev_c, dst_evt_c[dst_evt_c$dst %in% dst_gev_c$dst, 4:7])

# Calculate the mean cluster size in daily maximum
theta_gev_a <- extremal((dst_gev_a$dst > 100), dst_gev_a$day) # 0.3257916
theta_gev_b <- extremal((dst_gev_b$dst > 100), dst_gev_b$day) # 0.337963
theta_gev_c <- extremal((dst_gev_c$dst > 100), dst_gev_c$day) # 0.3886503
theta_gev_avg <- sum(c(theta_gev_a, theta_gev_b, theta_gev_c))/3 # 0.3508016

# Fit the GEV model

# Stationary model
fit_gev_1 <- gev.fit(dst_gev_a$dst) # llh = -2954.186

# Non-stationary model
m_gev_2 <- list(dst ~ bz_gsm, ~ 1, ~ 1)
fit_gev_2 <- evgam(m_gev_2, dst_gev_a, family = "gev") # llh = -2947.536

m_gev_3 <- list(dst ~ bz_gsm + density + speed + temperature, ~ 1, ~ 1)
fit_gev_3 <- evgam(m_gev_3, dst_gev_a, family = "gev") # llh = -2859.919

m_gev_4 <- list(dst ~ bz_gsm + density + speed + temperature, 
                ~ density + speed + temperature, ~ 1)
fit_gev_4 <- evgam(m_gev_4, dst_gev_a, family = "gev") # llh = -2836.713

m_gev_5 <- list(dst ~ bz_gsm + density + speed + temperature, 
                ~ density + speed + temperature, ~ bz_gsm)
fit_gev_5 <- evgam(m_gev_5, dst_gev_a, family = "gev") # llh = -2836.636

# Model diagnosis with return levels
diag_gev <- function(model, idx, plot_type=0, data=dst_gev_a, theta=theta_gev_avg) {
        coef <- predict(model, data, type="response")
        norm_z <- (1/coef$shape) * log(1 + coef$shape * (
            data$dst - coef$location)/coef$scale)
        n <- length(norm_z)
        
        # Probability plot
        if (plot_type=="p") {
            plot((1:n)/(n+1), exp(-exp(-sort(norm_z))), 
                 main=paste('GEV Model', idx), xlab="", ylab="")
            lines(0:1,0:1)
        }
        
        # Quantile plot
        if (plot_type=="q") {
            plot(-log(-log((1:n)/(n+1))), sort(norm_z), 
                 main=paste('Log Lik:', round(model$logLik, 3)), 
                 xlab="", ylab="")
            lines(0:100,0:100)
        }

        # Compute the 5-, 10- and 100- year return levels
        rl_5 <- qev(0.8, coef$location, coef$scale, coef$shape, 
                     m = 365, theta = theta, family = "gev")
        rl_10 <- qev(0.9, coef$location, coef$scale, coef$shape, 
                     m = 365, theta = theta, family = "gev")
        rl_100 <- qev(0.99, coef$location, coef$scale, coef$shape, 
                     m = 365, theta = theta, family = "gev")
        c(rl_5, rl_10, rl_100)
}

gev.diag(fit_gev_1) # 5-/10-/100-yr return level: 40.03936 - 55.0663 - 110.442

oldpar <- par(mfrow=c(2,4))
for (i in c("p", "q")) {
    diag_gev(fit_gev_2, 2, plot_type=i) # 151.9108 - 173.5570 - 250.8165
    diag_gev(fit_gev_3, 3, plot_type=i) # 146.9784 - 169.1064 - 251.6470
    diag_gev(fit_gev_4, 4, plot_type=i) # 187.9604 - 232.3723 - 471.6108
    diag_gev(fit_gev_5, 5, plot_type=i) # 188.4813 - 232.9358 - 465.4228
}
par(oldpar)

# Likelihood Ratio Test
LRT(nrow(dst_gev_a), 10, 7, -2836.713, -2859.919) # Model 3 vs 4
LRT(nrow(dst_gev_a), 11, 10, -2836.636, -2836.713) # Model 4 vs 5

# Average empirical return level
sum(-Dst_raw_a$dst > 187.9604)/(nrow(Dst_raw_a)/24/365) # 5-year level: 20.3622
sum(-Dst_raw_b$dst > 187.9604)/(nrow(Dst_raw_b)/24/365) # 5-year level: 0.6663624
sum(-Dst_raw_c$dst > 187.9604)/(nrow(Dst_raw_c)/24/365) # 5-year level: 3.446223

sum(-Dst_raw_a$dst > 232.3723)/(nrow(Dst_raw_a)/24/365) # 10-year level: 9.117402
sum(-Dst_raw_b$dst > 232.3723)/(nrow(Dst_raw_b)/24/365) # 10-year level: 0
sum(-Dst_raw_c$dst > 232.3723)/(nrow(Dst_raw_c)/24/365) # 10-year level: 2.247537

sum(-Dst_raw_a$dst > 471.6108)/(nrow(Dst_raw_a)/24/365) # 100-year level: 0
sum(-Dst_raw_b$dst > 471.6108)/(nrow(Dst_raw_b)/24/365) # 100-year level: 0
sum(-Dst_raw_c$dst > 471.6108)/(nrow(Dst_raw_c)/24/365) # 100-year level: 0

oldpar <- par(mfrow=c(1,3))
plot(dst_gev_a$dst, main="Daily Maximum of Denoised Dst Index - a", 
     xlab="Time (Day)", ylab="-Dst (nT)", sub="(a)")
legend("topleft", legend=c('theta_gev_a=0.326'), bty="n")
abline(h=100, col=2)

plot(dst_gev_b$dst, main="Daily Maximum of Denoised Dst Index - b", 
     xlab="Time (Day)", ylab="-Dst (nT)", sub="(b)")
legend("topleft", legend=c('theta_gev_b=0.334'), bty="n")
abline(h=100, col=2)

plot(dst_gev_c$dst, main="Daily Maximum of Denoised Dst Index - c", 
     xlab="Time (Day)", ylab="-Dst (nT)", sub="(c)")
legend("topleft", legend=c('theta_gev_c=0.389'), bty="n")
abline(h=100, col=2)
par(oldpar)

# -------------------------------- GPD Models ----------------------------------

# Set a constant threshold - Strong storm
threshold <- 100

# Setting excesses corresponding to non-exceedances as NA 
#   ensures that only exceedances are modeled
dst_gpd_a <- dst_evt_a
dst_gpd_a$excess <- dst_evt_a$dst - threshold
is.na(dst_gpd_a$excess[dst_gpd_a$excess <= 0]) <- TRUE

dst_gpd_b <- dst_evt_b
dst_gpd_b$excess <- dst_evt_b$dst - threshold
is.na(dst_gpd_b$excess[dst_gpd_b$excess <= 0]) <- TRUE

dst_gpd_c <- dst_evt_c
dst_gpd_c$excess <- dst_evt_c$dst - threshold
is.na(dst_gpd_c$excess[dst_gpd_c$excess <= 0]) <- TRUE

# Calculate the empirical mean cluster size (daily cluster)
oldpar <- par(mfrow=c(1,3))
plot(dst_gpd_a$excess, xlab="", ylab="excess", main="A")
plot(dst_gpd_b$excess, xlab="", ylab="excess", main="B")
plot(dst_gpd_c$excess, xlab="", ylab="excess", main="C")
par(oldpar)

n_cluster_a <- 9
theta_gpd_a <- n_cluster_a/sum(!is.na(dst_gpd_a$excess)) # 0.09375

n_cluster_b <- 5
theta_gpd_b <- n_cluster_b/sum(!is.na(dst_gpd_b$excess)) # 0.0877193

n_cluster_c <- 6
theta_gpd_c <- n_cluster_c/sum(!is.na(dst_gpd_c$excess)) # 0.04137931

theta_gpd_avg <- sum(theta_gpd_a, theta_gpd_b, theta_gpd_c)/3 # 0.07428287

# Fit the GPD model

# Stationary model
fit_gpd_1 <- gpd.fit(dst_gpd_a$dst, threshold, npy=365*24) # llh = -396.6056

# Non-stationary model
m_gpd_2 <- list(excess ~ bz_gsm, ~ 1)
fit_gpd_2 <- evgam(m_gpd_2, dst_gpd_a, family = "gpd") # llh = -392.8526

m_gpd_3 <- list(excess ~ bz_gsm + density + speed + temperature, ~ 1)
fit_gpd_3 <- evgam(m_gpd_3, dst_gpd_a, family = "gpd") # llh = -375.7775

m_gpd_4 <- list(excess ~ bz_gsm + density + speed + temperature, ~ bz_gsm)
fit_gpd_4 <- evgam(m_gpd_4, dst_gpd_a, family = "gpd") # llh = -366.0471

# Model diagnosis with return levels
diag_gpd <- function(model, idx, plot_type=0, threshold = 100, 
                     data=dst_gpd_a, theta=theta_gpd_avg) {
    coef <- predict(model, data, type="response")
    norm_y <- na.omit((1/coef$shape) * log(1 + coef$shape * (
        data$excess)/coef$scale))
    n <- length(norm_y)
    
    # Probability plot
    if (plot_type=="p") {
        plot((1:n)/(n+1), 1-exp(-sort(norm_y)), 
             main=paste('GPD Model', idx), xlab="", ylab="")
        lines(0:1,0:1)
    }
    
    # Quantile plot
    if (plot_type=="q") {
        plot(-log(1-((1:n)/(n+1))), sort(norm_y), 
             main=paste('Log Lik:', round(model$logLik, 3)), 
             xlab="", ylab="")
        lines(0:100,0:100)
    }
    
    # Compute the 5-, 10- and 100- year return levels
    rl_5 <- qev(0.8, threshold, coef$scale, coef$shape, 
                 m = 365*24, theta = theta, family = "gpd")
    rl_10 <- qev(0.9, threshold, coef$scale, coef$shape, 
                 m = 365*24, theta = theta, family = "gpd")
    rl_100 <- qev(0.99, threshold, coef$scale, coef$shape, 
                  m = 365*24, theta = theta, family = "gpd")
    c(rl_5, rl_10, rl_100)
}

gpd.diag(fit_gpd_1) # 136.5738 - 153.0998 - 211.5679

oldpar <- par(mfrow=c(2,3))
for (i in c("p", "q")) {
    diag_gpd(fit_gpd_2, 2, plot_type=i) # 233.2588 - 246.6125 - 294.9219
    diag_gpd(fit_gpd_3, 3, plot_type=i) # 204.5496 - 230.3172 - 328.1820
    diag_gpd(fit_gpd_4, 4, plot_type=i) # 208.0023 - 240.2553 - 445.9142
}
par(oldpar)

# Likelihood Ratio Test
LRT(nrow(dst_gpd_a), 3, 2, -392.8526, -396.6056) # Model 1 vs 2
LRT(nrow(dst_gpd_a), 6, 3, -375.7775, -392.8526) # Model 2 vs 3
LRT(nrow(dst_gpd_a), 7, 6, -366.0471, -375.7775) # Model 3 vs 4

# -------------------- GPD Models with varying threshold -----------------------

# Set threshold as the 99th percentile
zeta <- 0.01
dst_gpd_vu <- dst_evt_a
dst_gpd_vu$cyc <- as.integer(dst_gpd_vu$day) %% 180

# Estimate thresholds and get excesses
m_ald <- list(dst ~ s(cyc, bs = "cc", k = 10), ~ s(cyc, bs = "cc"))
fit_ald <- evgam(m_ald, dst_gpd_vu, family = "ald", 
                 ald.args = list(tau = 1- zeta))

dst_gpd_vu$threshold <- predict(fit_ald)$location
dst_gpd_vu$excess <- dst_gpd_vu$dst- dst_gpd_vu$threshold
is.na(dst_gpd_vu$excess[dst_gpd_vu$excess <= 0]) <- TRUE

plot(dst_gpd_vu$dst, main="", xlab="Time", ylab="-Dst (nT)", xaxt = "n", cex=0.75)
lines(dst_gpd_vu$threshold, col = "red")

# Fit threshold varying GPD model
m_gpd_5 <- list(excess ~ bz_gsm + density + speed + temperature + 
                    s(cyc, bs = "cc", k = 15), ~ bz_gsm)
fit_gpd_5 <- evgam(m_gpd_5, dst_gpd_vu, family = "gpd")

# Get theta = 1/mean cluster size
theta_gpd_uv <- extremal(!is.na(dst_gpd_vu$excess)) # 0.1250371

# Model diagnosis with return levels
oldpar <- par(mfrow=c(2,4))
for (i in c("p", "q")) {
    diag_gpd(fit_gpd_2, 2, plot_type=i) 
    diag_gpd(fit_gpd_3, 3, plot_type=i) 
    diag_gpd(fit_gpd_4, 4, plot_type=i) 
    diag_gpd(fit_gpd_5, 5, plot_type=i, 
             threshold=dst_gpd_vu$threshold, 
             data=dst_gpd_vu, 
             theta=theta_gpd_uv) # 226.4598 - 234.3776 - 280.8919
}
par(oldpar)

# Likelihood Ratio Test
LRT(nrow(dst_gpd_a), 20, 7, -392.748, -366.0471) # Model 4 vs 5

# Average empirical return level
sum(-Dst_raw_a$dst > 208.0023)/(nrow(Dst_raw_a)/24/365) # 5-year level: 13.37219
sum(-Dst_raw_b$dst > 208.0023)/(nrow(Dst_raw_b)/24/365) # 5-year level: 0.3331812
sum(-Dst_raw_c$dst > 208.0023)/(nrow(Dst_raw_c)/24/365) # 5-year level: 3.146552

sum(-Dst_raw_a$dst > 240.2553)/(nrow(Dst_raw_a)/24/365) # 10-year level: 7.901749
sum(-Dst_raw_b$dst > 240.2553)/(nrow(Dst_raw_b)/24/365) # 10-year level: 0
sum(-Dst_raw_c$dst > 240.2553)/(nrow(Dst_raw_c)/24/365) # 10-year level: 1.79803

sum(-Dst_raw_a$dst > 445.9142)/(nrow(Dst_raw_a)/24/365) # 100-year level: 0
sum(-Dst_raw_b$dst > 445.9142)/(nrow(Dst_raw_b)/24/365) # 100-year level: 0
sum(-Dst_raw_c$dst > 445.9142)/(nrow(Dst_raw_c)/24/365) # 100-year level: 0

oldpar <- par(mfrow=c(1,3))
plot((1:nrow(Dst_raw_a))/24, -Dst_raw_a$dst, 
     main="5-, 10-, and 100-year return level - a", 
     xlab="Time (Day)", ylab="-Dst (nT)", 
     ylim=range(c(-Dst_raw_a$dst, 500)), cex=0.5)
abline(h=187.9604, col=2)
abline(h=232.3723, col=2)
abline(h=471.6108, col=2)
abline(h=208.0023, col=4)
abline(h=240.2553, col=4)
abline(h=445.9142, col=4)

par(new = TRUE)
plot((1:nrow(Dst_raw_a))/24, rep(ssn_a, each=24), type = 'l', 
     lty=2, axes = FALSE, bty = "n", xlab = "", ylab = "")
axis(side=4, at = as.integer(ssn_a/20)*20)

plot((1:nrow(Dst_raw_b))/24, -Dst_raw_b$dst, 
     main="5-, 10-, and 100-year return level - b", 
     xlab="Time (Day)", ylab="-Dst (nT)", 
     ylim=range(c(-Dst_raw_b$dst, 500)), cex=0.5)
abline(h=187.9604, col=2)
abline(h=232.3723, col=2)
abline(h=471.6108, col=2)
abline(h=208.0023, col=4)
abline(h=240.2553, col=4)
abline(h=445.9142, col=4)

par(new = TRUE)
plot((1:nrow(Dst_raw_b))/24, rep(ssn_b, each=24), type = 'l', 
     lty=2, axes = FALSE, bty = "n", xlab = "", ylab = "")
axis(side=4, at = as.integer(ssn_b/20)*20)

plot((1:nrow(Dst_raw_c))/24, -Dst_raw_c$dst,
     main="5-, 10-, and 100-year return level - c", 
     xlab="Time (Day)", ylab="-Dst (nT)", 
     ylim=range(c(-Dst_raw_c$dst, 500)), cex=0.5)
abline(h=187.9604, col=2)
abline(h=232.3723, col=2)
abline(h=471.6108, col=2)
abline(h=208.0023, col=4)
abline(h=240.2553, col=4)
abline(h=445.9142, col=4)
legend("topleft", legend=c('GEV Model 4', 'GPD Model 4', 'Sun Spot Number',
                           'Return Levels  (gev/gpd):',
                           '5-yr:187.9604 / 208.0023',
                           '10-yr: 232.3723 / 240.2553',
                           '100-yr: 471.6108 / 445.9142'), 
       col=c(2,4,1,0,0,0,0), lty=c(1,1,2,0,0,0,0), bty="n")

par(new = TRUE)
plot((1:nrow(Dst_raw_c))/24, rep(ssn_c, each=24), type = 'l', 
     lty=2, axes = FALSE, bty = "n", xlab = "", ylab = "")
axis(side=4, at = as.integer(ssn_c/20)*20)
par(oldpar)








