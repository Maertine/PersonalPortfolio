

0.25*1+0.5*2+0.25*3 #16-19
0.5*1+0.25*2+0.05*3 #20-22
0.3*1+0.1*2 #23-27
0.4*1 #28-30

-0.5*1 #28-30
-0.3*1-0.1*2 #31-33
-0.5*1-0.25*2 #34-37
-0.25*1-0.5*2-0.25*3 #38+

sample(c(0,1,2,3),prob = c(0,0.25,0.5,0.25),size=1)

TALENT100 <- matrix(c(rep(c(0,0.05,0.45,0.5),4),rep(c(0,0.4,0.5,0.1),3),rep(c(0.3,0.5,0.2,0),5),rep(c(0.2,0.8,0,0),3)),ncol=4,byrow = TRUE)
TALENT0 <- matrix(c(rep(c(0,0.45,0.55,0),4),rep(c(0.4,0.6,0,0),3),rep(c(0.9,0.1,0,0),5),rep(c(1,0,0,0),3)),ncol=4,byrow = TRUE)
TALENTDIFF <- TALENT100 - TALENT0

DOWN <- matrix(c(rep(c(0.5,0.5,0,0),3),rep(c(0.6,0.3,0.1,0),3),rep(c(0.25,0.5,0.25,0),4),rep(c(0,0.25,0.5,0.25),4)),ncol=4,byrow = TRUE)


single_progression <- function(s) {
  if (s == 50) {}
  else if (s == 49) {s = s + sample(c(0,1),prob=c(0.67,0.33),size=1)}
  else if (s == 48) {s = s + sample(c(0,1),prob=c(0.34,0.66),size=1)}
  else if (s == 47) {s = s + sample(c(0,1),prob=c(0.01,0.99),size=1)}
  else {s = s + 1}
  return(s)
}

progression <- function(talent, fs, ts, start_age, end_age = 30) {
  skills <- c(fs+ts)
  for (t in start_age:end_age) {
    if (t<31) {
      UP <- TALENT0 + TALENTDIFF * talent * 0.01
      probs <- UP[t - 15, ]
      delta <- sample(c(0, 1, 2, 3), prob = probs, size = 1)
      if (delta > 0) {
        for (i in seq_len(delta)) {
          if (fs == 50) {ts = single_progression(ts)}
          else if (ts == 50) {fs = single_progression(fs)}
          else if (sample(c(0,1),size=1)==1) {fs = single_progression(fs)}
          else {ts = single_progression(ts)}
          }
      }}
    
    if (t>27) {
      probs <- DOWN[t - 27, ]
      lambda <- sample(c(0, 1, 2, 3), prob = probs, size = 1)
      if (lambda > 0) {
        for (i in seq_len(lambda)) {
          if (sample(c(0,1),size=1)==1) {ts = ts - 1} else {fs = fs - 1}
    }}}
    
    skills <- c(skills, fs+ts)
  }
  return(skills)
}

one_plot <- function(fs,ts,start_age,end_age) {
  replicates <- replicate(10000, progression(sample(1:100,size=1), fs, ts, start_age, end_age)/2)
  means <- rowMeans(replicates)
  q25 <- apply(replicates, 1, quantile, probs = 0.25)
  q75 <- apply(replicates, 1, quantile, probs = 0.75)
  mins <- apply(replicates, 1, min)
  maxs <- apply(replicates, 1, max)
  
  plot(start_age:(end_age+1), means, type = "l", lwd = 2, col = "blue", ylim = range(mins, maxs),
       ylab = "Skill", xlab = "Age", main = "Skill Progression Summary")
  lines(start_age:(end_age+1), q25, col = "gray", lty = 2)
  lines(start_age:(end_age+1), q75, col = "gray", lty = 2)
  lines(start_age:(end_age+1), mins, col = "red", lty = 3)
  lines(start_age:(end_age+1), maxs, col = "red", lty = 3)
  lines(c(0,100),c(27.14,27.14), col = "darkgreen", lty = 3)
  #lines(c(0,100),c(45.5,45.5), col = "orange", lty = 3)
  legend("bottomleft", legend = c("Mean", "25–75%", "Min/Max"),
         col = c("blue", "gray", "red"), lty = c(1, 2, 3), bty = "n")
}

one_plot(28,28,16,40)
