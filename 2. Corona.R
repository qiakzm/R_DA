#코로나 확진자 수 분석하기
# 샘플 데이터 : 경상북도 확진자 수 (2020.02 ~ 2022.02)
# 출처 : https://www.data.go.kr/data/15099351/fileData.do

# 1. 공공 데이터 불러오기. (맥북 한글 깨짐 현상 해결)

co <- read.csv("ccc.csv", fileEncoding = "euc-kr")
co

# 한글이 깨지지 않고 잘 나오는 것을 확인할 수 있다.
# 이는 맥북에서는 csv 저장 방식이 UTF-8이 아닌 euc-kr로 저장하기 때문에 인코딩 문제가 발생한다.
# 그래서 read.csv("~.csv") 뒤에 인코딩을 직접 명시해준다.
