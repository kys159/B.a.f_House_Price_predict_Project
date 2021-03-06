# :house: House Price Predict using Linear Regression :house: <br>
###### 비어플 홈페이지 주소 : https://www.dgubaf.com/

<br>

### 2018.12 비어플 프로젝트
&nbsp;&nbsp; 비어플 세션을 통해 배운내용들을 바탕으로 실제 데이터를 분석해보는 프로젝트입니다. 미국 에임스 도시 집들의 정보를 가지고 다른 집들의 판매가격을 예측하는것이 목표입니다.<br>
kaggle에서 제공되어진 데이터이며 리더보드를 통해 서로 경쟁이 진행되었던 데이터입니다.

<br>

## :bulb: 전체적인 분석목표
 - **Linear Regression**
   + 많은 머신러닝, 딥러닝 기법들이 존재하나 여전히 준수한 예측력을 보이며 특히 **해석가능한 모형**이라는 장점을 가진 선형회귀분석을 활용하여 집값 예측에서 더 나아가 어떤 요인이 집값에 영향을 미치는지 파악하고자 합니다.<br>
 - 프로젝트 당시 2학년 2학기로 아직 선형회귀분석에 대해 배우지 않은 상태였으며 비어플 세션에서 배운 선형회귀분석 내용을 바탕으로 실제 데이터에 적용시켜보고 모형의 해석까지 나아가는것이 목표입니다.<br>
 - 집값 상승에 영향을 미치는 요인을 파악한다면 집 판매에 관심있는 사람들에게 중요한 정보가 될 수 있을 것입니다.

<br>

## :file_folder: 파일 구조
```
├── House_Price_Project/
   ├── Preprocessing1.R
   ├── Preprocessing2.R
   └── Modeling.R
```
 - `Preprocessing1.R` 데이터를 불러오기, 파생변수 생성, 이상치 처리등의 전처리 내용이 담겨있는 파일입니다.
 - `Preprocessing2.R` 1과 동일합니다.
 - `Modeling.R` 최종 데이터를 활용하여 Linear Regression 모델을 학습시키는 파일입니다.
 
 <br>
 
 <img src="https://user-images.githubusercontent.com/61648914/90131156-a58cd000-dda6-11ea-8a81-dda5e494d0b4.png" width="50%" height="30%" title="px(픽셀) 크기 설정"><img src="https://user-images.githubusercontent.com/61648914/90131258-cce39d00-dda6-11ea-922c-edf34f5d5af3.png" width="50%" height="30%" title="px(픽셀) 크기 설정">
 
 <br>
 
 ## :disappointed: 아쉬운점
  - 아무래도 처음 접해보는 데이터 분석이다보니 데이터 분할, 모델링 등에서 미숙한 점이 많았으나 그만큼 많이 배울 수 있는 분석이였습니다. <br>
  - 면적이 중요하지 않은 변수로 나온 이유는 단위문제인 것으로 보임. 1/100 등의 처리를 하여 단위를 맞춰주면 좋은 변수로 사용할 수 있을것으로 보임. <br>
  - 아노바 대신 Type 3 Sum Of Square를 활용하여 범주형 변수를 제거할지에 대한 여부를 확인할 수 있다. <br>
  



 
 
