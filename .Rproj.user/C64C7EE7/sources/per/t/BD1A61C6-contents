
## 정형 분석 파일의 종류
# Database ( oracle mysql mariaDb )
# Exel (xls)
# CSV ( Comma Separated Vector)


## 외부 library 를 갖다 쓰는 것.

# 디플라이어 - text 분석의 최강 
install.packages("dplyr") 

# 호출할 때는 이미 설치된 객체이기 때문에 따옴표 쓰지 않아도 된다. 
library(dplyr)



read.csv("class_scores.csv")

# temp <- c(read.csv("class_scores.csv"))
# temp <- list(read.csv("class_scores.csv"))

scores <- data.frame(read.csv("class_scores.csv"))
scores

head(scores)
tail(scores)
summary(scores)
dim(scores)
View(scores)


# 스키마 명 저장 
# Stu_ID scores class gender Math English Science Marketing Writing


## dplyr
## select : 선택한 meta data에 해당하는 instance를 출력. 
## filter, distance, top_n, sample_n : 선택한 row - key vlaue 에 해당하는 instance 를 출력.

## select 는 열 탐색 - 컬럼
## filter 는 행 탐색 - 로우 

# mutate, transmutate, mutate_each : data 형태 변환 
# group_by : group data
# summarise, summarise_each, count : summary - 요약 
# arrange : datda sorting - 정렬 
# inner_join, left_join, right_join, full_join : 데이터 결합/조합 



# mean, max, min
head(select(scores, "Math"))
mean(select(scores, "Math"))
select(scores, "Math")

filter(scores, "Math")





########################################
## select 예제 
########################################
## 1 과학, 영어, 수학 도메인기(=컬럼)만 가져오기

scores %>% 
  dplyr::select(Math, English, Science) %>%
  head


## 2 상위 10개 보기

scores %>% 
  dplyr::select(Math, English, Science) %>%
  slice(1:10)


## 3 성별 제외한 컬럼 보기

scores %>%
  dplyr::select(-gender) %>%
  slice(1:10)



## 4 수학부터 작문까지 컬럼 보기

scores %>%
  dplyr::select(Math:Writing) %>%
  slice(1:10)


## 5 모든 컬럼 조회

scores %>%
  dplyr::select(everything()) %>%
  slice(1:10)


## 6 E 로 시작하는 컬럼만 보기
starts_with('E')

scores %>%
  dplyr::select(starts_with('E')) %>%
  slice(1:10)



## 7 e 로 끝나는 컬럼만 보기
ends_with('e')

scores %>%
  dplyr::select(ends_with('e')) %>%
  slice(1:10)



## 8 e 가 들어가는 컬럼 다 가져오기
contains('e')


scores %>%
  dplyr::select(contains('e')) %>%
  slice(1:10)



## 9.  1, 3, 5번째 컬럼만 가져오기

scores %>%
  dplyr::select(1,3,5) %>%
  slice(1:10)




########################################
## filter 예제
########################################
## 1. 1학년 학생들만 보기

scores %>%
  dplyr::filter(grade==1) %>%
  slice(1:10)


## 2. 1학년 남학생만 보기

scores %>%
  dplyr::filter(grade==1 & gender=='M') %>%   ## comma available 
  slice(1:10)


## 3. 1학년이 아닌 학생들만 보기

scores %>%
  dplyr::filter(grade != 1) %>%
  slice(1:10)


scores %>%
  dplyr::filter(!grade== 1) %>%
  slice(1:10)


## 4. 1, 2학년 학생들만 보기 

scores %>%
  dplyr::filter(grade == 1 | grade == 2) %>%
  tail


## 5. 수학점수가 80이상인 학생들만 보기

scores %>%
  dplyr::filter(Math >= 80) %>%
  slice(1:10)



## 6. 수학점수가 80 이상이면서 영어점수가
##     70이상이 학생들만 보기

scores %>%
  dplyr::filter(Math >= 80 & English >=70) %>%
  slice(1:10)



## 7. 학번이 10101 부터 10120인 학생들
##    중에서 여학생이면서 영어가 80점 이상인
##    학생만 보기

scores %>%
  dplyr::filter(Stu_ID >= 10101 & Stu_ID <= 10120 & gender == 'F' & English >= 80) %>%
  slice(1:10)


## 8. 학번이 홀수인 학생들 중 남자이면서 
##    수학과 과학이 모두 90점 이상인
##    학생들만 보기

scores %>%
  dplyr::filter((Stu_ID%%2 != 0) & gender == 'M' & Math >= 90 & Science >= 90) %>%
  slice(1:10)



## 9. 학생들 중 한 과목이라도 100점이 있는
##    학생만 보기

scores %>%
  dplyr::filter(Math == 100 | English == 100 | Science == 100 | Marketing == 100 | Writing == 100) %>%
  slice(1:10)



## 10. 학생들 중 한 과목이라도 0점이 있는
##     학생만 보기

scores %>%
  dplyr::filter(Math == 0 | English == 0 | Science == 0 | Marketing == 0 | Writing == 0) %>%
  slice(1:10)





########################################
## mutate 예제 - 컬럼 추가 하는 것 !
########################################

## 1. scores 의 Average 컬럼(학생 평균 점수) 추가 

scores_avg <- data.frame(
                scores %>%
                  dplyr::mutate(Average = (Math+English+Science+Marketing+Writing)%/%5)
              )
scores_avg


scores <- scores %>%
            dplyr::mutate(Average = (Math+English+Science+Marketing+Writing)%/%5)

View(scores)


## 2. 학생들 평균 점수를 기준으로 Rank(순위) 추가

scores <- scores %>% 
            dplyr::mutate(Rank = dense_rank(desc(Average)))


View(scores)


## 3. Average 를 기준으로 정렬하기
## arrange(Average) 

scores %>%
  dplyr::arrange(Rank) %>%
  slice(1:10)


## ifelse 예제
## 4. 평균점수가 90점 이상이면 'A'
    ## 80점 이상이면 'B'
    ## 70점 이상이면 'C'
    ## 60점 이상이면 'D'
    ## 50점 이상이면 'E'
    ## 나머지는 'F'

scores <- scores %>%
  dplyr::mutate(Score = ifelse(Average >= 90, 'A', ifelse(Average >= 80, 'B', ifelse(Average >= 70, 'C', ifelse(Average >= 60, 'D', ifelse(Average >= 50, 'E', 'F'))))))

View(scores)




## group_by 예제 
## 학생별 학생수 보기

scores %>%
  dplyr::group_by(grade) %>%
  dplyr::summarise(Count = length(grade))


## 평균점수를 성별로 보기

temp <- scores %>%
  dplyr::group_by(gender) %>%
  dplyr::summarise(gender_mean = mean(Average))

# 히스토그램
hist(temp$gender_mean,
     xlab="남",
     col="yellow",
     border="blue")


# 바차트 
barplot(temp$gender_mean)

#파이차트
pie(
  c(temp$gender_mean),
  c("남", "여"),
  col=c("blue", "red")
)


# 라인 차트
plot(c(temp$gender_mean), type="o")


