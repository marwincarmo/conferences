library(ivd)
library(data.table)

school_ses <- saeb[, .(school_ses = mean(student_ses, na.rm = TRUE)), by = school_id]

# Join the school_ses back to the original dataset
saeb <- saeb[school_ses, on = "school_id"]

# Grand mean center school ses
saeb$school_ses <- c(scale(saeb$school_ses, scale = FALSE))

head(saeb )

out <- ivd(location_formula = math_proficiency ~ student_ses * school_ses + (1|school_id),
           scale_formula =  ~ student_ses * school_ses + (1|school_id),
           data = saeb,
           niter = 2000, nburnin = 7000, WAIC = TRUE, workers = 4)
saveRDS(out, "out/out.rds")
summary(out)
