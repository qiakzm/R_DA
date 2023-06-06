# 연관분석

# arulesViz 패키지를 설치하고 Groceries 데이터를 활용해보자

library(arulesViz)
data("Groceries")
summary(Groceries)

# 최소 지지도 0.001, 최소 신뢰도 0.5 이상의 연관규칙 생성
rules <- apriori(Groceries, parameter=list(support=0.001, confidence=0.5))
rules
# inspect()함수로 리프트 기준으로 상위 3개의 생성 규칙 확인
inspect(head(sort(rules,by="lift"),3))

# plot 그려보기(점들이 겹치지 않게 = jitter)
plot(rules)
plot(rules, jitter=0)

# 향상도 기준으로 상위 3개의 규칙을 생성하고 생선된 연관규칙을 시각화
subrules2 <- head(sort(rules, by="lift"),3)
plot(subrules2, method="graph", control=list(type = "item"))
