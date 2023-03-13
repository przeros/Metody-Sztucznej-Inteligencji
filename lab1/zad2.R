library(mlbench)
data(Sonar) #wczytanie zbioru danych do środowiska. Dokumentację wywołać można poleceniem ?Sonar
summary(Sonar) #proste podsumowanie danych, przedziały kwartylowe

boxplot(Sonar, main = "Charakterystyka wartości cech w zbiorze danych Sonar", ylim = c(0, 2))
boxplot(Sonar[Sonar$Class == "R", ], main = "Charakterystyka wartości cech dla klasy R w zbiorze danych Sonar", ylim = c(0, 2))
boxplot(Sonar[Sonar$Class == "M", ], main = "Charakterystyka wartości cech dla klasy M w zbiorze danych Sonar", ylim = c(0, 2))

plot(Sonar$Class, Sonar$V12) #plot rozpoznaje typ danych Class - jakościowe i prezentuje wykresy typu boxplot
plot(Sonar$V12, Sonar$Class) #pseudokodowanie klass, M=2, R=1. Widoczny charakter rozkładów wartości różny dla obu klas

r = Sonar$V12[Sonar$Class == "R"]
m = Sonar$V12[Sonar$Class == "M"]
#wyznaczone statystyki wartości, do użycia w modelu gęstości prawdopodobieństwa
mr = mean(r)

sdr = sd(r)
mm = mean(m)
sdm = sd(m)
sprintf(
  "Średnia i odchylenie dla klasy %s równe są: mean %0.2f sd %0.4f",
  c("R", "M"),
  c(mr, mm),
  c(sdr, sdm)
)
hist(r)
hist(m)

x = seq(0, 0.7, length = 1000)
mteoret = dnorm(x, mean = mm, sd = sdm)
mteoret = mteoret / sum(mteoret) #normalizacja, tak by całe pole pod wykresem było równe 1.
rteoret = dnorm(x, mean = mr, sd = sdr)
rteoret = rteoret / sum(rteoret)
plot(
  x,
  rteoret,
  ylab = "Prawdopodobieństwo",
  xlab = "Wartość cechy V12",
  main = "Prawdopodobieństwo dla klasy R-czarne, M-czerwone",
  type = "l",
  col = "black"
)
points(x, mteoret, type = "l", col = "red")

prog = 0.244
points(c(prog, prog), c(0, 1), col = "black", type = "l")
points(x[x > prog], rteoret[x > prog], pch = 4, col = "black")
points(x[x <= prog], mteoret[x <= prog], pch = 4, col = "red")

liczbaR = sum(Sonar$Class == "R") #sum tutaj zlicza przypadki gdy warunek jest TRUE
liczbaM = sum(Sonar$Class == "M")

R.T = sum(Sonar$V129[Sonar$Class == "R"] <= prog) #sum tutaj zlicza przypadki gdy cały warunek jest TRUE, czyli wiersze gdzie klasa to R oraz wartości są poniżej progu
R.TPR = R.T / liczbaR #true positive rate

M.T = sum(Sonar$V12[Sonar$Class == "M"] > prog)
M.TPR = M.T / liczbaM #true positive rate

sprintf(
  "Dla progu decyzyjnego V12 = %0.3f true positive rate dla klasy %s wynosi %0.2f",
  c(prog),
  c("R", "M"),
  c(R.TPR, M.TPR)
)

R.F = sum(Sonar$V12[Sonar$Class == "R"] > prog) #czarne krzyżyki
R.FNR = R.F / liczbaR #false negative rate

M.F = sum(Sonar$V12[Sonar$Class == "M"] <= prog) #czerwone krzyżyki
M.FNR = M.F / liczbaM #true negative rate

sprintf(
  "Dla progu decyzyjnego V12 = %0.3f false negative rate dla klasy %s wynosi %0.2f",
  c(prog),
  c("R", "M"),
  c(R.FNR, M.FNR)
)