#install.packages("readxl")
library(readxl)
#EDA 데이터탐색
#데이터불러오기
dat <- read_excel("chargingspot.xlsx")
#데이터모양확인
head(dat)
tail(dat)
#데이터 통계량 확인
summary(dat) 
#자료구조,관측치, 자료형 확인
str(dat)
#결측치 확인
is.na(dat)
sum(is.na(dat))


dat1 = dat[,-1]
dat1

#시도별 충전소 수를 H,L로 나누기 위한 grp변수추가
grp <- c()
for (i in 1:nrow(dat1)){  #dat1$충전소 값에 따라 그룹 분류
  if (dat1$충전소[i] >= 259.3){#298.6은 mean 값임, 220은 median 값임
    #중앙값과 평균의 중간값인 259.3이용
    grp[i] <- "H"} else {grp[i] <- "L"}}


grp <-factor(grp)  #문자벡터를 팩터 타입으로 변경
grp <- factor(grp, levels=c("H","L"))  #레벨의 순서를 H,L -> H,L

dat2 <- data.frame(dat1, grp)  #dat에 grp 컬럼 추가
#추가된데이터 확인
str(dat2)
head(dat2)
table(dat2$grp)

#두 개의 그룹의 평균벡터가 차이가 나는지 안나는지
groupH <- dat2[dat2$grp == "H", -14] # High
groupL <- dat2[dat2$grp == "L", -14] # Low

colMeans(groupH)
colMeans(groupL)

library(Hotelling)#mean vector
#귀무가설 : 두개의 mean vector가 동일하다
#p-value가 0.05보다 작으면 귀무가설 기각

result <- hotelling.test(x = groupH, y = groupL)
result
#################
#두 그룹의 Covariance Matrix가 동일한지 아닌지
round(cov(groupH), 2) # High
round(cov(groupL), 2) # Low

library(heplots)

result <- boxM(cbind(전기차대수,대학교,문화시설,의료기관병상수,경제활동인구,
                     인구, 공원면적) ~ grp, data = dat2)
result


#H, L 각 그룹별 관측값 분포 확인
par(mfrow=c(2,2))
for(i in 1:13) {
  boxplot(dat2[,i]~dat2$grp,main=colnames(dat2)[i])
}


#######################
dat_cor <- round(cor(dat1),2)
dat_cor
#히트맵
par(mfrow=c(1,1))
library(corrplot)
corrplot(dat_cor)
#숫자형태로
corrplot(dat_cor, method="number")



fit_pca <- princomp(dat1, cor = TRUE)
fit_pca$sdev^2
fit_pca$loadings
dat1
summary(fit_pca)
screeplot(fit_pca, npcs = 13, type = "lines", main = "scree plot")
biplot(fit_pca)

library(factoextra)
fviz_eig(fit_pca)

summary(fit_pca)
fviz_contrib(fit_pca, choice = "var", axes = 1) #PC1영향받는것
fviz_contrib(fit_pca, choice = "var", axes = 2) #PC2영향받는것
fviz_contrib(fit_pca, choice = "var", axes = 3) #PC3영향받는것
fviz_contrib(fit_pca, choice = "var", axes = 4) #PC4영향받는것


fviz_pca_var(fit_pca,
             col.var = "cos2", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot( fit_pca,
                 repel = TRUE,
                 geom = c("point"),
                 col.var = "#2E9FDF", # Variables color
                 col.ind = "#696969"  # Individuals color
)

#회귀분석
dat1
model_1 = lm(충전소 ~ 전기차대수 + 대학교 + 문화시설 + 의료기관병상수 + 
               경제활동인구 + 인구 + 공원면적 + 순이동 + 공공의료기관수 + 
               지역내총생산 + 논밭면적 + 도로보급, data=dat1)
summary(model_1)

#유의한 독립변수로만 회귀분석 다시 진행
model_2 = lm(충전소 ~ 경제활동인구 + 인구 + 순이동 + 공공의료기관수,
             data=dat1)
summary(model_2)

#변수 제거 전, 후 모형 비교
anova(model_1, model_2)

#변수의 상대적 중요도를 시각화
relweights <- function(fit,...){
  R <- cor(fit$model)
  nvar <- ncol(R)
  rxx <- R[2:nvar, 2:nvar]
  rxy <- R[2:nvar, 1]
  svd <- eigen(rxx)
  evec <- svd$vectors
  ev <- svd$values
  delta <- diag(sqrt(ev))
  lambda <- evec %*% delta %*% t(evec)
  lambdasq <- lambda ^ 2
  beta <- solve(lambda) %*% rxy
  rsquare <- colSums(beta ^ 2)
  rawwgt <- lambdasq %*% beta ^ 2
  import <- (rawwgt / rsquare) * 100
  import <- as.data.frame(import)
  row.names(import) <- names(fit$model[2:nvar])
  names(import) <- "Weights"
  import <- import[order(import),1, drop=FALSE]
  dotchart(import$Weights, labels=row.names(import),
           xlab="% of R-Square", pch=19,
           main="Relative Importance of Predictor Variables",
           sub=paste("Total R-Square=", round(rsquare, digits=3)),
           ...)
  return(import)
}

pp1 = relweights(model_1, col="blue")
pp1
# 데이터
dataa <- c(0.8957698, 0.9911657, 1.4491809, 2.7209284, 2.8514528, 3.5512724, 10.0816763, 13.7223408, 13.8046443, 16.2418965, 16.3779688, 17.3117032)
labels <- c("순이동", "지역내총생산", "문화시설", "공원면적", "의료기관병상수", "논밭면적", "도로보급", "공공의료기관수", "대학교", "인구", "경제활동인구", "전기차대수")
# 색상 팔레트 설정
colors <- rainbow(length(dataa))
# 총합 계산
total <- sum(dataa)
# 퍼센트 계산 및 라벨 설정
percent <- round(dataa / total * 100, 2)
labels_with_percent <- paste(labels, percent, "%", sep = " ")
# 원 그래프 그리기
pie(dataa, labels = labels_with_percent, col = colors)

pp = relweights(model_2, col="blue")
pp

# 데이터
dataa <- c(1.24919, 35.52300, 32.78764, 33.44017)
labels <- c("순이동","인구","경제활동인구", "공공의료기관수")
# 색상 팔레트 설정
colors <- rainbow(length(dataa))
# 총합 계산
total <- sum(dataa)
# 퍼센트 계산 및 라벨 설정
percent <- round(dataa / total * 100, 2)
labels_with_percent <- paste(labels, percent, "%", sep = " ")
# 원 그래프 그리기
pie(dataa, labels = labels_with_percent, col = colors)

#####입지순위선정
# 변수 정규화 함수
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# 입지 순위 계산 함수
calculate_rank <- function(data, weights) {
  normalized_data <- apply(data, 2, normalize)
  weighted_average <- normalized_data %*% weights
  rank <- rank(-weighted_average, ties.method = "min")
  return(rank)
}

# 변수들과 가중치 설정
variables <- c("경제활동인구", "인구", "순이동", "공공의료기관수")
weights <- c(0.5967332, -0.0003018, -0.0298973, 7.0797779)
data <- dat1[,c("경제활동인구","인구", "순이동", "공공의료기관수")]
data<-data.frame(data)
data

# 입지 순위 계산
rank <- calculate_rank(data, weights)

# 결과 출력
result <- data.frame("행정구역(시도)" = row.names(data), "입지순위" = rank)
print(result)
