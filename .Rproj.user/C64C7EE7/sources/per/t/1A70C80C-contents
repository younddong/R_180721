## 외부에서 접속해서 분석할 경우..
## getwd()
## dir.create("경로")


### RStudio 와 Oracle 연결
install.packages("rJava")
install.packages("DBI")
install.packages("RJDBC")
library(rJava)
library(DBI)
library(RJDBC)

## jdbc 드라이버 로딩 
## 1차 java - oracle 연결 
drv <- JDBC(
  driverClass = "oracle.jdbc.driver.OracleDriver",
  classPath = "C:\\oraclexe\\app\\oracle\\product\\11.2.0\\server\\jdbc\\lib\\ojdbc6.jar"
)

## 2차 oracle - R 연결 
conn <- dbConnect(drv,
                  "jdbc:oracle:thin:@localhost:1521:xe",
                  "hr",
                  "hr")

dbGetQuery(conn, "SELECT * FROM TABS")

employees <- data.frame(dbGetQuery(conn, "SELECT * FROM employees"))
dbGetQuery(conn, "SELECT * FROM employees")
dbGetQuery(conn, "SELECT * FROM locations")

dbGetQuery(conn, "SELECT tname FROM tab")


employees
View(employees)




### install package
install.packages("data.table")
install.packages("dplyr")
library(data.table)
library(dplyr)



tab <- dbGetQuery(conn, "SELECT * FROM TAB")
View(tab)

# $ --> list에서 하나 가져올 때 / 열 가져옴
tname <- tab$TNAME
tname
  # 
  # cnt : COUNTRIES
  # dep : DEPARTMENTS
  # emp : EMPLOYEES
  # empd : EMP_DETAILS_VIEW
  # job : JOBS
  # jobh : JOB_HISTORY
  # loc : LOCATIONS
  # reg : REGIONS
  #


cnt <- data.frame(dbGetQuery(conn, "select * from COUNTRIES"))
View(cnt)

dep <- data.frame(dbGetQuery(conn, "select * from DEPARTMENTS"))
View(dep)

emp <- data.frame(dbGetQuery(conn, "select * from EMPLOYEES"))
View(emp)

empd <- data.frame(dbGetQuery(conn, "select * from EMP_DETAILS_VIEW"))
View(empd)

job <- data.frame(dbGetQuery(conn, "select * from JOBS"))
View(job)

jobh <- data.frame(dbGetQuery(conn, "select * from JOB_HISTORY"))
View(jobh)

loc <- data.frame(dbGetQuery(conn, "select * from LOCATIONS"))
View(loc)

reg <- data.frame(dbGetQuery(conn, "select * from REGIONS"))
View(reg)


emp %>%
  select(everything())

  # 
  # EMPLOYEE_ID
  # FIRST_NAME
  # LAST_NAME
  # EMAIL
  # PHONE_NUMBER
  # HIRE_DATE
  # JOB_ID
  # SALARY
  # COMMISSION_PCT
  # MANAGER_ID
  # DEPARTMENT_ID
  #
  

## 1. 사원의 First Name 과 Last Name을 분여서 name으로 된 컬럼을 추가하시오 
## 단, 이름 간격은 띄울 것. ex) James Dean 

emp <- emp %>%
        mutate(Name = paste(emp$FIRST_NAME,' ',emp$LAST_NAME))

View(emp)


## 2. Salary 는 연봉(달러)를 말한다. 매달 지급하는 월급여를 MONTH_SAL 컬럼 추가

emp <- emp %>%
        mutate(MONTH_SAL = emp$SALARY/12)

View(emp)


## 3. 급여가 20000불 이상인 사원의 목록을 name, employee_id, salary 만 출력 

emp %>%
  filter(emp$SALARY >= 20000) %>%
  select(Name, EMPLOYEE_ID, SALARY)



## 4. 급여가 7000불 이하인 사원에게 보너스로 급여의 10%를 더 지급하겠다고 함.
## 이번달에 보너스가 추가된 대상자의 목록을 name, employee_id, salary 만 출력.

emp %>%
  filter(emp$SALARY <= 7000) %>%
  select(Name, EMPLOYEE_ID, SALARY)


## 5. 직원 중에서 성(LAST_NAME)에 e 또는 o 가 포함된 직원을 출력.
## apply 를 알아야 해결 가능 !

getCharE <- function(df) {
  return(df[grep('e', df)])
}

getCharO <- function(df) {
  return(df[grep('o', df)])
}

last_name_e <- apply(emp %>% 
                     select(LAST_NAME), 2, getCharE)

last_name_o <- apply(emp %>% 
                       select(LAST_NAME), 2, getCharO)

emp %>%
  filter(emp$LAST_NAME %in% last_name_e | emp$LAST_NAME %in% last_name_o) %>%
  select(LAST_NAME)




## 6. 직원 중에서 급여가 가장 높은 사람이 CEO 라고 함. 이름이 뭘까?
## apply(object, direction, function to apply)
## 적용방향 --> 1 : 가로(로우) / 2 : 세로(컬럼)

ceo_sal <- apply(emp %>%
                   select(SALARY), 2, max)

emp %>%
  filter(SALARY == ceo_sal) %>%
  select(Name)

