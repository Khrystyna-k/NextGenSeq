setwd('/proj/users/khrystyna/Genotype_likelihoods')

beagle <- read.table('FILE_NAME.beagle.gz', header = T)

major_hom_column <- grep("Ind[0-9]+$", names(beagle))

beagle_copy <- beagle

for(mhc in major_hom_column){
 no_diff_rows <- beagle[,mhc] == beagle[,mhc + 1] & beagle[,mhc] == beagle[,mhc + 2]
 beagle_copy[no_diff_rows, mhc + c(0,1,2)] <- NA
}

beagle_copy$n_Ind <- rowSums(!is.na(all_morphs_39_NA[, major_hom_column]))
beagle_copy$exp_major_count <- rowSums(beagle_copy[, major_hom_column] + beagle_copy[, major_hom_column + 1]*0.5, na.rm =T )
beagle_copy$exp_majnor_freq <- 1-(beagle_copy$exp_major_count)/beagle_copy$n_Ind

#Estimate allele frequencies
beagle_copy[, c(1:3, 4:72)]$n_Ind_myv <- rowSums(!is.na(beagle_copy[, major_hom_column]))
beagle_copy[, c(1:3, 4:72)]$exp_major_count_myv <- rowSums(beagle_copy[, major_hom_column] + beagle_copy[, major_hom_column + 1]*0.5, na.rm =T )
beagle_copy[, c(1:3, 4:72)]$exp_majnor_freq_myv <- 1-(beagle_copy$exp_major_count)/beagle_copy$n_Ind
