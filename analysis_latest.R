# program to analyze Google Forms survey Likert Scale
# questions

if(!require(tidyverse)){install.packages('tidyverse')}
if(!require(FSA)){install.packages('FSA')}
if(!require(psych)){install.packages("psych")}
if(!require(likert)){install.packages("likert")}
if(!require(waffle)){install.packages("waffle")}
if(!require(rstatix)){install.packages("rstatix")}
if(!require(GPArotation)){install.packages("GPArotation")}
if(!require(amap)){install.packages("amap")}
if(!require(lavaan)){install.packages("lavaan")}
if(!require(cluster)){install.packages("cluster")}
if(!require(factoextra)){install.packages("factoextra")}
if(!require(dendextend)){install.packages("dendextend")}
if(!require(MultNonParam)){install.packages("MultNonParam")}

library(tidyverse)
library(psych)
library(likert)
library(FSA)
library(waffle)
library(rstatix)
library(GPArotation)
library(amap)
library(lavaan)
library(cluster)
library(factoextra)
library(dendextend)
library(MultNonParam)

getwd()
setwd()
getwd()

data <- read.csv("final_data.csv")
summary(data)

# Monte Carlo Test for Statistical Power

# kwsamplesize(c(1:5), 'normal', targetpower = .8, level = .1)
# # requires group samples of 14
# kwpower(rep(5,5),c(1:5),"normal", level = .1, mc = 10000)
# # power ~.95

## cannot use alpha of .05, but .1 will work

# eliminate duplicate entries as erroneous entry
data_unique <- unique(data)
data <- data_unique

data$gpa_diff <- (data$est_current_gpa - data$pre_home_gpa)
data$study_diff <- (data$post_study_hrs - data$pre_study_hrs)

# waffle charts and bar chart

bar_height <- table(data$grade_level)
grade_names <- c('Freshmen', 'Sophomores', 'Juniors', 'Seniors')
bar_colors = c('#d7191c', '#fdae61', '#abdda4', '#2b83ba')
barplot(bar_height, xlab = 'Grade', ylab = 'Number of Responses',
        ylim = c(0, 20) ,axes = TRUE,
        main = 'Distribution of Grades Amongst Responses', 
        names.arg = grade_names, col = bar_colors)

likert <- data[2:14]

div_colors = c('#d7191c', '#fdae61', '#bebada', '#abdda4', '#2b83ba')

likert %>% count(likert$class_less_conf) -> class_less_conf
class_less_conf <- c('Strongly Disagree' = class_less_conf[1,2],
                     'Disagree' = class_less_conf[2,2], 
                      'Neutral' = class_less_conf[3,2],
                      'Agree'= class_less_conf[4,2],
                      'Strongly Agree' = class_less_conf[5,2])
waffle(class_less_conf, rows = 8, size = 1, legend_pos = 'right',
       colors = div_colors,
       title = 'The change in style of my math class \nmakes me feel less confident in my ability to \nsucceed in math.')

likert %>% count(likert$teacher_quality) -> teacher_quality
teacher_quality <- c('Strongly Disagree' = teacher_quality[1,2],
                     'Disagree' = teacher_quality[2,2], 
                     'Neutral' = teacher_quality[3,2],
                     'Agree'= teacher_quality[4,2],
                     'Strongly Agree' = teacher_quality[5,2])
waffle(teacher_quality, rows = 8,size = 1, legend_pos = 'right',
       colors = div_colors,
       title = 'My math teacher has done an excellent job \ndeveloping an online curriculum for us.')

likert %>% count(likert$no_support) -> no_support
no_support <- c('Strongly Disagree' = no_support[1,2],
                     'Disagree' = no_support[2,2], 
                     'Neutral' = no_support[3,2],
                     'Agree'= no_support[4,2],
                     'Strongly Agree' = no_support[5,2])
waffle(no_support, rows = 8,size = 1, legend_pos = 'right',
       colors = div_colors,
       title = 'I no longer received the math support \nI needed when I began distance learning.')

likert %>% count(likert$online_need) -> online_need
online_need <- c('Strongly Disagree' = online_need[1,2],
                'Disagree' = online_need[2,2], 
                'Neutral' = online_need[3,2],
                'Agree'= online_need[4,2],
                'Strongly Agree' = online_need[5,2])
waffle(online_need, rows = 8,size = 1, legend_pos = 'right',
       colors = div_colors,
       title = 'To keep up in class, I feel the need \nto use other online learning tools to study.')

likert %>% count(likert$peer_drop) -> peer_drop
peer_drop <- c('Strongly Disagree' = peer_drop[1,2],
                 'Disagree' = peer_drop[2,2], 
                 'Neutral' = peer_drop[3,2],
                 'Agree'= peer_drop[4,2],
                 'Strongly Agree' = peer_drop[5,2])
waffle(peer_drop, rows = 8,size = 1, legend_pos = 'right',
       colors = div_colors,
       title = 'I feel that the lack of peer interaction \nhas caused my math performance to suffer.')

likert %>% count(likert$online_pref) -> online_pref
online_pref <- c('Strongly Disagree' = online_pref[1,2],
                   'Disagree' = online_pref[2,2], 
                   'Neutral' = online_pref[3,2],
                   'Agree'= online_pref[4,2],
                   'Strongly Agree' = online_pref[5,2])
waffle(online_pref, rows = 8,size = 1, legend_pos = 'right',
       colors = div_colors,
       title = 'Online learning is not my preferred learning style.')

likert %>% count(likert$flex_pref) -> flex_pref
flex_pref <- c('Strongly Disagree' = flex_pref[1,2],
                 'Disagree' = flex_pref[2,2], 
                 'Neutral' = flex_pref[3,2],
                 'Agree'= flex_pref[4,2],
                 'Strongly Agree' = flex_pref[5,2])
waffle(flex_pref, rows = 8,size = 1, legend_pos = 'right',
       colors = div_colors,
       title = 'The flexibility of online instruction \nallows me to perform better in my math class.')

likert %>% count(likert$exam_anxiety) -> exam_anxiety
exam_anxiety <- c('Strongly Disagree' = exam_anxiety[1,2],
               'Disagree' = exam_anxiety[2,2], 
               'Neutral' = exam_anxiety[3,2],
               'Agree'= exam_anxiety[4,2],
               'Strongly Agree' = exam_anxiety[5,2])
waffle(exam_anxiety, rows = 8,size = 1, legend_pos = 'right',
       colors = div_colors,
       title = 'Online exams cause me more anxiety \nthan in-person exams did.')

likert %>% count(likert$non_exam_weight) -> non_exam_weight
non_exam_weight <- c('Strongly Disagree' = non_exam_weight[1,2],
                  'Disagree' = non_exam_weight[2,2], 
                  'Neutral' = non_exam_weight[3,2],
                  'Agree'= non_exam_weight[4,2],
                  'Strongly Agree' = non_exam_weight[5,2])
waffle(non_exam_weight, rows = 8,size = 1, legend_pos = 'right',
       colors = div_colors,
       title = 'My math teacher has made it clear that \nwhile exams are important, our completed \ncoursework and involvement will do \nmore for my final grade.')

likert %>% count(likert$study_difficulty) -> study_difficulty
study_difficulty <- c('Strongly Disagree' = study_difficulty[1,2],
                  'Disagree' = study_difficulty[2,2], 
                  'Neutral' = study_difficulty[3,2],
                  'Agree'= study_difficulty[4,2],
                  'Strongly Agree' = study_difficulty[5,2])
waffle(study_difficulty, rows = 8,size = 1, legend_pos = 'right',
       colors = div_colors,
       title = 'It is more difficult for me to study before exams \nor start homework early now than it was during \nin-class instruction.')

# generate factors

data$grade_level <- factor(data$grade_level,
                                levels = c("9", "10", "11", "12"),
                                ordered = TRUE)

data$math_conf <- factor(data$math_conf,
                                levels = c("1", "2", "3", "4", "5"),
                                ordered = TRUE)

data$class_less_conf <- factor(data$class_less_conf,
                                      levels = c("1", "2", "3", "4", "5"),
                                      ordered = TRUE)

data$teacher_quality <- factor(data$teacher_quality,
                                      levels = c("1", "2", "3", "4", "5"),
                                      ordered = TRUE)

data$no_support <- factor(data$no_support,
                                 levels = c("1", "2", "3", "4", "5"),
                                 ordered = TRUE)

data$online_need <- factor(data$online_need,
                                  levels = c("1", "2", "3", "4", "5"),
                                  ordered = TRUE)

data$peer_drop <- factor(data$peer_drop,
                                levels = c("1", "2", "3", "4", "5"),
                                ordered = TRUE)

data$better_online <- factor(data$better_online,
                                    levels = c("1", "2", "3", "4", "5"),
                                    ordered = TRUE)

data$online_pref <- factor(data$online_pref,
                                  levels = c("1", "2", "3", "4", "5"),
                                  ordered = TRUE)

data$flex_pref <- factor(data$flex_pref,
                                levels = c("1", "2", "3", "4", "5"),
                                ordered = TRUE)

data$online_worry <- factor(data$online_worry,
                                   levels = c("1", "2", "3", "4", "5"),
                                   ordered = TRUE)

data$exam_anxiety <- factor(data$exam_anxiety,
                                   levels = c("1", "2", "3", "4", "5"),
                                   ordered = TRUE)

data$non_exam_weight <- factor(data$non_exam_weight,
                                      levels = c("1", "2", "3", "4", "5"),
                                      ordered = TRUE)

data$study_difficulty <- factor(data$study_difficulty,
                                       levels = c("1", "2", "3", "4", "5"),
                                       ordered = TRUE)

# kruskall-wallis rank sum test with dunn post-hoc using
# benjamini-hochberg method for adjusting p-values and
# kruskal-wallis effect size test

# Q3
kruskal.test(gpa_diff ~ class_less_conf, data = data)
dunnTest(gpa_diff ~ class_less_conf, data = data, method = 'bh')
data %>% kruskal_effsize(gpa_diff ~ class_less_conf, conf.level = .9)

# Q5
kruskal.test(gpa_diff ~ no_support, data = data)
dunnTest(gpa_diff ~ no_support, data = data, method = 'bh')
data %>% kruskal_effsize(gpa_diff ~ no_support, conf.level = .9)

kruskal.test(study_diff ~ no_support, data = data)
dunnTest(study_diff ~ no_support, data = data, method = 'bh')
data %>% kruskal_effsize(study_diff ~ no_support, conf.level = .9)

# Q6
kruskal.test(gpa_diff ~ online_need, data = data)
dunnTest(gpa_diff ~ online_need, data = data, method = 'bh')
data %>% kruskal_effsize(gpa_diff ~ online_need, conf.level = .9)

kruskal.test(study_diff ~ online_need, data = data)
dunnTest(study_diff ~ online_need, data = data, method = 'bh')
data %>% kruskal_effsize(study_diff ~ online_need, conf.level = .9)

# Q7
kruskal.test(gpa_diff ~ peer_drop, data = data)
dunnTest(gpa_diff ~ peer_drop, data = data, method = 'bh')
data %>% kruskal_effsize(gpa_diff ~ peer_drop, conf.level = .9)

# Q8
kruskal.test(gpa_diff ~ better_online, data = data)
dunnTest(gpa_diff ~ better_online, data = data, method = 'bh')
data %>% kruskal_effsize(gpa_diff ~ better_online, conf.level = .9)

# Q9
kruskal.test(gpa_diff ~ online_pref, data = data)
dunnTest(gpa_diff ~ online_pref, data = data, method = 'bh')
data %>% kruskal_effsize(gpa_diff ~ online_pref, conf.level = .9)

kruskal.test(study_diff ~ online_pref, data = data)
dunnTest(study_diff ~ online_pref, data = data, method = 'bh')
data %>% kruskal_effsize(study_diff ~ online_pref, conf.level = .9)

# Q10
kruskal.test(gpa_diff ~ flex_pref, data = data)
dunnTest(gpa_diff ~ flex_pref, data = data, method = 'bh')
data %>% kruskal_effsize(gpa_diff ~ flex_pref, conf.level = .9)

kruskal.test(study_diff ~ flex_pref, data = data)
dunnTest(study_diff ~ flex_pref, data = data, method = 'bh')
data %>% kruskal_effsize(study_diff ~ flex_pref, conf.level = .9)

# Q12
kruskal.test(gpa_diff ~ exam_anxiety, data = data)
dunnTest(gpa_diff ~ exam_anxiety, data = data, method = 'bh')
data %>% kruskal_effsize(gpa_diff ~ exam_anxiety, conf.level = .9)

# Q13
kruskal.test(gpa_diff ~ non_exam_weight, data = data)
dunnTest(gpa_diff ~ non_exam_weight, data = data, method = 'bh')
data %>% kruskal_effsize(gpa_diff ~ non_exam_weight, conf.level = .9)

# Q14
kruskal.test(gpa_diff ~ study_difficulty, data = data)
dunnTest(gpa_diff ~ study_difficulty, data = data, method = 'bh')
data %>% kruskal_effsize(gpa_diff ~ study_difficulty, conf.level = .9)

# correlation between gpa change and study habits change
cor(data$gpa_diff, data$study_diff)

# attempts to group responses together

data_cor <- mutate_all(data, function(x) as.numeric(as.character(x)))
data_cor_cols <- c(3:7, 9, 10, 12:14)
data_cor <- data_cor[,data_cor_cols]

data_cor <- data_cor %>% rename(
        Less_Confident = class_less_conf,
        Good_Curriculum = teacher_quality,
        Need_More_Support = no_support,
        Need_Online_Tools = online_need,
        Lack_Peers_Interact = peer_drop,
        Online_Not_Prefer = online_pref,
        Flexibility_Helps = flex_pref,
        More_Exam_Anxiety = exam_anxiety,
        Not_Summative = non_exam_weight,
        Hard_to_Study = study_difficulty
)

poly_data_cols <- -c(12,13)
poly_cor_data <- data_cor[,poly_data_cols]
poly_cor <- polychoric(poly_cor_data)
# par(mfrow=c(1,1))
# par(mar=c(5,5,8,5))

cor.data <- data_cor %>% cor_mat()
cor.data %>%
        cor_reorder() %>%
        pull_lower_triangle() %>%
        cor_plot(method = 'shade', label = TRUE, insignificant = 'cross')

cor.plot(poly_cor$rho, numbers = TRUE, main = 'Polychoric Correlation',
         upper = FALSE, show.legend = FALSE, xlas = 3)

# determine number of factors for grouping
fa.parallel(rho, fm='pa', fa='both', nfactors = factor_number ,main = 'Scree Plot')
nfactors(data_cor)
factor_number = 5

iclust(data_cor, nclusters = factor_number)

omega(data_cor, nfactors = factor_number, n.obs = 71)

omegaDirect(data_cor, nfactors = factor_number)

omegaSem(data_cor, nfactors = factor_number)

fa <- fa(data_cor, factor_number, n.obs = 71)
fa.diagram(fa, main = 'Factor Analysis')

# hierarchical clustering to explore data

levels = unique(do.call(c, data_cor)) #all unique values in df
out <- sapply(levels, function(x)colSums(data_cor==x)) #count occurrences of x in each row
colnames(out) <- levels

likert_sums <- as.data.frame(out[,c(4, 1, 2, 3, 5)])
likert_sums_t <- t(likert_sums)

k_groups = 5
dist_1 <- dist(likert_sums, method = 'euclidean')
tree <- hclust(dist_1, method = 'ward.D2')

par(mar=c(7,3,1,1))
k_colors = c('#d7191c', '#fdae61', '#bebada', '#abdda4', '#2b83ba')
dend <-  as.dendrogram(tree) %>%
        set("branches_lwd", 2) %>% 
        set("branches_k_color", k_colors, k = k_groups) %>% 
        set("labels_colors", k_colors, k = k_groups) %>% 
        set("labels_cex", .8) %>%
        set("leaves_col", k_colors)
plot(dend)




