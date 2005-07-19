hiv <-
  structure(list(means = structure(
                   c(0,0,0,0,0,0),
                   names = c("immunoglobin G", "immunoglobin A", "lymphocyte B",
                     "platelet count", "lymphocyte T4",
                     "T4/T8 lymphocyte ratio")),
                 stddev = structure(
                   c(2.97,0.44,2987.35,142.80,1397.42,1.17),
                   names = c("immunoglobin G", "immunoglobin A", "lymphocyte B",
                     "platelet count", "lymphocyte T4",
                     "T4/T8 lymphocyte ratio")),
                 corr = structure(matrix(c(
                   1.0000, 0.4829, 0.2198,-0.0398, 0.2526,-0.2757,
                   0.4829, 1.0000, 0.0572,-0.1328,-0.1242,-0.3144,
                   0.2198, 0.0572, 1.0000, 0.1491, 0.5227,-0.1834,
                   -0.0398,-0.1328, 0.1491, 1.0000, 0.1794, 0.0639,
                   0.2526,-0.1242, 0.5227, 0.1794, 1.0000, 0.2126,
                   -0.2757,-0.3144,-0.1834, 0.0639, 0.2126, 1.0000
                   ), 6,6),            
                   dimnames = list(
                     c("immunoglobin G", "immunoglobin A", "lymphocyte B",
                       "platelet count", "lymphocyte T4", "T4/T8 lymphocyte ratio"),
                     c("immunoglobin G", "immunoglobin A", "lymphocyte B",
                       "platelet count", "lymphocyte T4", "T4/T8 lymphocyte ratio"))),
                 n = 107))
## from Roverato/Whittaker (Hyper normal ...bayes factors, 1996)

  
  
