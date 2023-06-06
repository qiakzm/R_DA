#클러스터링

#R로K-means 클러스터링은 매우 쉽다. kmeans()함수만 호출하면 된다.

member <- data.frame(spent = c(10,1,1,17,4,6,1,15,22,3,0,3,7,0,2),
                     time = c(15,2,10,7,5,7,1,10,18,3,1,3,7,10,2))
# 게임을 이용하는 사람들이 얼마나 돈을 사용하고(spent), 시간을 투자(time)했는지에 대한 간단한 데이터이다.
# 위 member 데이터를 3개의 군집으로 나눠보자.

result <- kmeans(member, 3)
result

# 6,5,4 씩 클러스터가 생성되었다.

# 그래프로 클러스터링을 표현해보자.

library(fpc)
plotcluster(member, result$cluster, color=TRUE, shade=TRUE)
# plots 부분을 보면 같은 범주로 군집이 나뉜 것을 확인할 수 있다.

# 클러스트링 결과 확인하기.

result$withinss
# 클러스트링 결과값은 다음과 같다.
# cluster : 어느 클러스터에 속해있는가
# centers : 각 클러스터의 센터는?
# totss : 총 스퀘어의 합
# withiness : 응집도, 클러스터 내부 데이터의 뭉침 정도(작아야 좋다)
# tot.withiness : witniness를 모두 더한 값
# betweenss : 분리도, 각 클러스터 간 떨어짐 정도, (커야 좋다)
# size : 각 클러스터에 속한 데이터 수
# iter : 얼마나 반복했는가

# 클러스터로 기본적 계산 해보기
member$cluster <- result$cluster
aggregate( data = member, spent ~ cluster, mean)

# 클러스터를 몇개로 나눠야 할지
library(NbClust)
nc <- NbClust(iris[, 1:4], min.nc=2, max.nc=6, method="kmeans")
# 위 NbClust는 클러스터를 만들어보고 추천해주는 라이브러리 이다. 