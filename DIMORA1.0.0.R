

#modifica della funzione BASS.standard: 
#cambio titoli figure, esporto previsioni, 
#aggiunto parametro ous per lunghezza out-of-sample
BASS.standard <-
function (sales, method = "nls", prelimestimates = c(sum(sales) + 
    100, 0.01, 0.1), ous= 100, alpha = 0.05, display = T) 
{
    x <- NULL
    t <- seq(1, length(sales), by = 1)
    s <- sales
    c <- cumsum(s)
    ff <- function(t, m, p, q) {
        m * (1 - exp(-(p + q) * t))/(1 + q/p * exp(-(p + q) * 
            t))
    }
    ff1 <- function(t, par) {
        c - ff(t, par[1], par[2], par[3])
    }
    ff2 <- function(t, par) {
        ff(t, par[1], par[2], par[3])
    }
    zprimo <- function(t, m, p, q) {
        m * (p + q * (ff(t, m, p, q)/m)) * (1 - (ff(t, m, p, 
            q)/m))
    }
    if (method == "nls") {
        stime <- nls.lm(par = prelimestimates, fn = ff1, t = t)$par
        sssss <- signif(summary(nls.lm(par = prelimestimates, 
            fn = ff1, t = t))$coefficients, digits = 3)
        aa <- data.frame(summary(nls.lm(par = prelimestimates, 
            fn = ff1, t = t))$coefficients[, c(1, 2)], 0, 0, 
            0)
        names(aa) <- c("Estimate", "Std.Error", "Lower", "Upper", 
            "p-value")
        row.names(aa) <- c("m :", "p :", "q :")
        for (i in 1:NROW(aa)) {
            aa[i, c(3, 4)] <- aa[i, 1] + c(-1, 1) * qnorm(1 - 
                alpha/2) * aa[i, 2]
        }
        aa[, 5] <- sssss[, 4]
        res <- nls.lm(par = prelimestimates, fn = ff1, t = t)$fvec
    }
    else if (method == "optim") {
        mass <- sum(s) + 1000
        ff3 <- function(par) {
            ff(t, par[1], par[2], par[3])
        }
        max <- sum(s) + 10000
        stima_optim <- function(c) {
            f <- function(p) {
                sum((c - ff3(p))^2)
            }
            optim(par = prelimestimates, fn = f, method = "L-BFGS-B", 
                lower = c(1e-10, 1e-10, 1e-10), upper = c(mass, 
                  1, 1))$par
        }
        stime <- stima_optim(c)
        aa <- stime
        res <- c - ff2(t, aa)
    }
    if (display == T) {
        par(mfrow = c(1, 2), mar = c(5, 4, 4, 2))
        plot(t, c, main = "Cumulative", xlim = c(0, 
            max(t) + ous), ylim = c(0, ff2(max(t) + ous, stime)),         
            #c(0, sum(s) + sum(s) * 50/100), #bisogna modificare ylim?
            ylab = "Cumulative")
        curve(ff2(x, stime), add = T, col = 2, xlim = c(0, max(t) + 
            ous))
        plot(t, sales, main = "Instantaneous", xlim = c(0, 
            max(t) + ous), ylab = "Instantaneous")
        curve(zprimo(x, stime[1], stime[2], stime[3]), col = 2, 
            add = T, xlim = c(0, max(t) + ous))
        par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))
    }
    s.hat <- ff2(t, stime)
    tss <- sum((c - mean(s))^2)
    rss <- sum((c - s.hat)^2)
    r.squared <- 1 - rss/tss
    r.squared.adj <- 1 - ((1 - r.squared) * (length(s) - 1))/(length(s) - 
        1 - NROW(aa))
    list(Estimate = aa, Rsquared = r.squared, RsquaredAdj = r.squared.adj, 
        residuals = res, fitted=s.hat)
}


###################################################################
#modifica della funzione BASS.generalized
#cambio titoli figure, esporto previsioni, 
#aggiunto parametro ous per lunghezza out-of-sample


BASS.generalized <-
function (sales, shock = c("exp", "rett", "mixed", "armonic"), 
    nshock, prelimestimates, alpha = 0.05, ous=100,  display = T) 
{
    a <- b <- c <- a1 <- b1 <- c1 <- a2 <- b2 <- c2 <- a3 <- b3 <- c3 <- x <- NULL
    t <- seq(1, length(sales), by = 1)
    s <- sales
    c <- cumsum(sales)
    if (nshock == 1) {
        if (shock == "exp") {
            intx <- function(t, a1, b1, c1) {
                (t + c1 * (1/b1) * (exp(b1 * (t - a1)) - 1) * 
                  (t >= a1))
            }
            cat("################## Exponential shock ################## \n")
            xt <- function(t, a1, b1, c1) {
                1 + (c1 * exp(b1 * (t - a1))) * (t >= a1)
            }
        }
        else if (shock == "rett") {
            intx <- function(t, a1, b1, c1) {
                (t + c1 * (t - a1) * (a1 <= t) * (t <= b1) + 
                  c1 * (b1 - a1) * (b1 < t))
            }
            cat("################## Rectangular shock  ################## \n")
            xt <- function(t, a1, b1, c1) {
                1 + c1 * (t >= a1) * (t <= b1)
            }
        }
        else if (shock == "mixed") {
            intx <- function(t, a1, b1, c1) {
                t + c1 * (1/b1) * (exp(b1 * (t - a1)) - 1) * 
                  (t >= a1)
            }
            cat("################## Mixed shock ################## \n")
            xt <- function(t, a1, b1, c1) {
                1 + (c1 * exp(b1 * (t - a1))) * (t >= a1)
            }
        }
        else if (shock == "armonic") {
            intx <- function(t, a1, b1, c1) {
                t + c1 * ((b1 - a1)/(2 * pi)) * cos(2 * pi * 
                  ((t - a1)/(b1 - a1))) * (t >= a1) * (b <= b1)
            }
            cat("################## Armonic shock  ################## \n")
            xt <- function(t, a1, b1, c1) {
                1 + c1 * cos(2 * pi * ((t - a1)/(b1 - a1))) * 
                  (t >= a1) * (b <= b1)
            }
        }
        ff <- function(t, m, p, q, a1, b1, c1) {
            m * (1 - exp(-(p + q) * intx(t, a1, b1, c1)))/(1 + 
                (q/p) * exp(-(p + q) * intx(t, a1, b1, c1)))
        }
        ff1 <- function(t, par) {
            c - ff(t, par[1], par[2], par[3], par[4], par[5], 
                par[6])
        }
        ff2 <- function(t, par) {
            ff(t, par[1], par[2], par[3], par[4], par[5], par[6])
        }
        zprimo <- function(t, m, p, q, a1, b1, c1) {
            m * (p + q * (ff(t, m, p, q, a1, b1, c1)/m)) * (1 - 
                (ff(t, m, p, q, a1, b1, c1)/m)) * xt(t, a1, b1, 
                c1)
        }
        stime <- nls.lm(par = prelimestimates, fn = ff1, t = t)$par
        aa <- data.frame(summary(nls.lm(par = prelimestimates, 
            fn = ff1, t = t))$coefficients[, c(1, 2)], 0, 0, 
            0)
        names(aa) <- c("Estimate", "Std.Error", "Lower", "Upper", 
            "P-value")
        row.names(aa) <- c("m :", "p :", "q :", "a1 :", "b1 :", 
            "c1 :")
        for (i in 1:NROW(aa)) {
            aa[i, c(3, 4)] <- aa[i, 1] + c(-1, 1) * qnorm(1 - 
                alpha/2) * aa[i, 2]
        }
        sssss <- signif(summary(nls.lm(par = prelimestimates, 
            fn = ff1, t = t))$coefficients, digits = 3)
        aa[, 5] <- sssss[, 4]
        resid <- nls.lm(par = prelimestimates, fn = ff1, t = t)$fvec
        if (display == TRUE){
        par(mfrow = c(1, 2))
        plot(t, c, xlim = c(0, length(t) + 200), ylim = c(0, 
            sum(s) + sum(s) * 0.4), main = "Cumulative", 
            ylab = "Cumulative")
        curve(ff2(x, stime), add = T, col = 2)
        plot(t, sales, main = "Instantaneous", xlim = c(0, 
            length(t) + 200), ylab = "Instantaneous")
        curve(zprimo(x, stime[1], stime[2], stime[3], stime[4], 
            stime[5], stime[6]), col = 2, add = T)
        par(mfrow = c(1, 1))
        }
        s.hat <- ff2(t, stime)
        tss <- sum((c - mean(s))^2)
        rss <- sum((c - s.hat)^2)
        r.squared <- 1 - rss/tss
        r.squared.adj <- 1 - ((1 - r.squared) * (length(s) - 
            1))/(length(s) - 1 - NROW(aa))
        list(Estimate = aa, Rsquared = r.squared, RsquaredAdj = r.squared.adj, 
            RSS = rss, residuals = resid, fitted=s.hat)
    }
    else if (nshock == 2) {
        if (shock == "exp") {
            intx1 <- function(t, a1, b1, c1, a2, b2, c2) {
                t + c1 * (1/b1) * (exp(b1 * (t - a1)) - 1) * 
                  (t >= a1) + c2 * (1/b2) * (exp(b2 * (t - a2)) - 
                  1) * (t >= a2)
            }
            cat("################## Exponential shock ################## \n")
            xt1 <- function(t, a1, b1, c1, a2, b2, c2) {
                1 + (c1 * exp(b1 * (t - a1))) * (t >= a1) + (c2 * 
                  exp(b2 * (t - a2))) * (t >= a2)
            }
        }
        else if (shock == "rett") {
            intx1 <- function(t, a1, b1, c1, a2, b2, c2) {
                t + c1 * (t - a1) * (a1 <= t) * (t <= b1) + c1 * 
                  (b1 - a1) * (b1 < t) + c2 * (t - a2) * (a2 <= 
                  t) * (t <= b2) + c2 * (b2 - a2) * (b2 < t)
            }
            cat("################## Rectangular shock  ################## \n")
            xt1 <- function(t, a1, b1, c1, a2, b2, c2) {
                1 + c1 * (t >= a1) * (t <= b1) + c2 * (t >= a2) * 
                  (t <= b2)
            }
        }
        else if (shock == "mixed") {
            intx1 <- function(t, a1, b1, c1, a2, b2, c2) {
                t + (c1/b1) * (exp(b1 * (t - a1)) - 1) * (a1 <= 
                  t) + c2 * (t - a2) * (a2 <= t) * (t <= b2) + 
                  c2 * (b2 - a2) * (b2 < t)
            }
            cat("################## Mixed shock ################## \n")
            xt1 <- function(t, a1, b1, c1, a2, b2, c2) {
                1 + (c1 * exp(b1 * (t - a1))) * (t >= a1) + c2 * 
                  (t >= a2) * (t <= b2)
            }
        }
        else if (shock == "armonic") {
            xt1 <- function(t, a1, b1, c1, a2, b2, c2) {
                1 + c1 * cos(2 * pi * ((t - a1)/(b1 - a1))) * 
                  (t >= a1) * (b <= b1) + c2 * cos(2 * pi * ((t - 
                  a2)/(b2 - a2))) * (t >= a2) * (b <= b2)
            }
            intx1 <- function(t, a1, b1, c1, a2, b2, c2) {
                t + c1 * ((b1 - a1)/(2 * pi)) * cos(2 * pi * 
                  ((t - a1)/(b1 - a1))) * (t >= a1) * (b <= b1) + 
                  c2 * ((b2 - a2)/(2 * pi)) * cos(2 * pi * ((t - 
                    a2)/(b2 - a2))) * (t >= a2) * (b <= b2)
            }
            cat("################## Armonic shock  ################## \n")
        }
        ff0 <- function(t, m, p, q, a1, b1, c1, a2, b2, c2) {
            m * (1 - exp(-(p + q) * intx1(t, a1, b1, c1, a2, 
                b2, c2)))/(1 + (q/p) * exp(-(p + q) * intx1(t, 
                a1, b1, c1, a2, b2, c2)))
        }
        ff1 <- function(t, par) {
            c - ff0(t, par[1], par[2], par[3], par[4], par[5], 
                par[6], par[7], par[8], par[9])
        }
        ff2 <- function(t, par) {
            ff0(t, par[1], par[2], par[3], par[4], par[5], par[6], 
                par[7], par[8], par[9])
        }
        zprimo1 <- function(t, m, p, q, a1, b1, c1, a2, b2, c2) {
            m * (p + q * (ff0(t, m, p, q, a1, b1, c1, a2, b2, 
                c2)/m)) * (1 - (ff0(t, m, p, q, a1, b1, c1, a2, 
                b2, c2)/m)) * xt1(t, a1, b1, c1, a2, b2, c2)
        }
        stime <- nls.lm(par = prelimestimates, fn = ff1, t = t)$par
        aa <- data.frame(summary(nls.lm(par = prelimestimates, 
            fn = ff1, t = t))$coefficients[, c(1, 2)], 0, 0, 
            0)
        names(aa) <- c("Estimate", "Std.Error", "Lower", "Upper", 
            "P-value")
        row.names(aa) <- c("m :", "p :", "q :", "a1 :", "b1 :", 
            "c1 :", "a2 :", "b2 :", "c2 :")
        for (i in 1:NROW(aa)) {
            aa[i, c(3, 4)] <- aa[i, 1] + c(-1, 1) * qnorm(1 - 
                alpha/2) * aa[i, 2]
        }
        sssss <- signif(summary(nls.lm(par = prelimestimates, 
            fn = ff1, t = t))$coefficients, digits = 3)
        aa[, 5] <- sssss[, 4]
        resid <- nls.lm(par = prelimestimates, fn = ff1, t = t)$fvec
        if(display == TRUE){
        par(mfrow = c(1, 2))
        plot(t, c, xlim = c(0, length(t) + 100), ylim = c(0, 
            max(s) + 5000), main = "Cumulative")
        curve(ff2(x, stime), type = "l", add = T, col = 2)
        plot(t, sales, main = "Instantaneous")
        curve(zprimo1(x, stime[1], stime[2], stime[3], stime[4], 
            stime[5], stime[6], stime[7], stime[8], stime[9]), 
            col = 2, add = T)
        par(mfrow = c(1, 1))
        }
        s.hat <- ff2(t, stime)
        tss <- sum((c - mean(s))^2)
        rss <- sum((c - s.hat)^2)
        r.squared <- 1 - rss/tss
        r.squared.adj <- 1 - ((1 - r.squared) * (length(s) - 
            1))/(length(s) - 1 - NROW(aa))
        list(Estimate = aa, Rsquared = r.squared, RsquaredAdj = r.squared.adj, 
            RSS = rss, residuals = resid, fitted=s.hat)
    }
    else if (nshock == 3) {
        if (shock == "exp" | shock == "rett" | shock == "armonic") {
            if (shock == "exp") {
                intx2 <- function(t, a1, b1, c1, a2, b2, c2, 
                  a3, b3, c3) {
                  t + c1 * (1/b1) * (exp(b1 * (t - a1)) - 1) * 
                    (t >= a1) + c2 * (1/b2) * (exp(b2 * (t - 
                    a2)) - 1) * (t >= a2) + c3 * (1/b3) * (exp(b3 * 
                    (t - a3)) - 1) * (t >= a3)
                }
                cat("################## Esponential shock ################## \n")
                xt2 <- function(t, a1, b1, c1, a2, b2, c2, a3, 
                  b3, c3) {
                  1 + (c1 * exp(b1 * (t - a1))) * (t >= a1) + 
                    (c2 * exp(b2 * (t - a2))) * (t >= a2) + (c3 * 
                    exp(b3 * (t - a3))) * (t >= a3)
                }
            }
            else if (shock == "rett") {
                intx2 <- function(t, a1, b1, c1, a2, b2, c2, 
                  a3, b3, c3) {
                  (t + c1 * (t - a1) * (a1 <= t) * (t <= b1) + 
                    c1 * (b1 - a1) * (b1 < t) + c2 * (t - a2) * 
                    (a2 <= t) * (t <= b2) + c2 * (b2 - a2) * 
                    (b2 < t) + c3 * (t - a3) * (a3 <= t) * (t <= 
                    b3) + c3 * (b3 - a3) * (b3 < t))
                }
                cat("################## Rectangular shock  ################## \n")
                xt2 <- function(t, a1, b1, c1, a2, b2, c2, a3, 
                  b3, c3) {
                  1 + c1 * (t >= a1) * (t <= b1) + c2 * (t >= 
                    a2) * (t <= b2) + c3 * (t >= a3) * (t <= 
                    b3)
                }
            }
            else if (shock == "armonic") {
                xt2 <- function(t, a1, b1, c1, a2, b2, c2, a3, 
                  b3, c3) {
                  1 + c1 * cos(2 * pi * ((t - a1)/(b1 - a1))) * 
                    (t >= a1) * (b <= b1) + c2 * cos(2 * pi * 
                    ((t - a2)/(b2 - a2))) * (t >= a2) * (b <= 
                    b2) + c3 * cos(2 * pi * ((t - a3)/(b3 - a3))) * 
                    (t >= a3) * (b <= b3)
                }
                intx2 <- function(t, a1, b1, c1, a2, b2, c2, 
                  a3, b3, c3) {
                  t + c1 * ((b1 - a1)/(2 * pi)) * cos(2 * pi * 
                    ((t - a1)/(b1 - a1))) * (t >= a1) * (b <= 
                    b1) + c2 * ((b2 - a2)/(2 * pi)) * cos(2 * 
                    pi * ((t - a2)/(b2 - a2))) * (t >= a2) * 
                    (b <= b2) + c3 * ((b3 - a3)/(2 * pi)) * cos(2 * 
                    pi * ((t - a3)/(b3 - a3))) * (t >= a3) * 
                    (b <= b3)
                }
                cat("################## Armonic shock  ################## \n")
            }
            ff00 <- function(t, m, p, q, a1, b1, c1, a2, b2, 
                c2, a3, b3, c3) {
                m * (1 - exp(-(p + q) * intx2(t, a1, b1, c1, 
                  a2, b2, c2, a3, b3, c3)))/(1 + (q/p) * exp(-(p + 
                  q) * intx2(t, a1, b1, c1, a2, b2, c2, a3, b3, 
                  c3)))
            }
            ff1 <- function(t, par) {
                c - ff00(t, par[1], par[2], par[3], par[4], par[5], 
                  par[6], par[7], par[8], par[9], par[10], par[11], 
                  par[12])
            }
            ff2 <- function(t, par) {
                ff00(t, par[1], par[2], par[3], par[4], par[5], 
                  par[6], par[7], par[8], par[9], par[10], par[11], 
                  par[12])
            }
            zprimo2 <- function(t, m, p, q, a1, b1, c1, a2, b2, 
                c2, a3, b3, c3) {
                m * (p + q * (ff00(t, m, p, q, a1, b1, c1, a2, 
                  b2, c2, a3, b3, c3)/m)) * (1 - (ff00(t, m, 
                  p, q, a1, b1, c1, a2, b2, c2, a3, b3, c3)/m)) * 
                  xt2(t, a1, b1, c1, a2, b2, c2, a3, b3, c3)
            }
            stime <- nls.lm(par = prelimestimates, fn = ff1, 
                t = t)$par
            aa <- data.frame(summary(nls.lm(par = prelimestimates, 
                fn = ff1, t = t))$coefficients[, c(1, 2)], 0, 
                0, 0)
            names(aa) <- c("Estimate", "Std.Error", "Lower", 
                "Upper", "P-value")
            row.names(aa) <- c("m :", "p :", "q :", "a1 :", "b1 :", 
                "c1 :", "a2 :", "b2 :", "c2 :", "a3 :", "b3 :", 
                "c3 :")
            for (i in 1:NROW(aa)) {
                aa[i, c(3, 4)] <- aa[i, 1] + c(-1, 1) * qnorm(1 - 
                  alpha/2) * aa[i, 2]
            }
            sssss <- signif(summary(nls.lm(par = prelimestimates, 
                fn = ff1, t = t))$coefficients, digits = 3)
            aa[, 5] <- sssss[, 4]
            resid <- nls.lm(par = prelimestimates, fn = ff1, 
                t = t)$fvec
            if (display == TRUE){
            par(mfrow = c(1, 2))
            plot(t, c, xlim = c(0, length(t) + 100), ylim = c(0, 
                max(s) + 5000), main = "Cumulative")
            curve(ff2(x, stime), type = "l", add = T, col = 2)
            plot(t, sales, main = "Instantaneous")
            curve(zprimo2(x, stime[1], stime[2], stime[3], stime[4], 
                stime[5], stime[6], stime[7], stime[8], stime[9], 
                stime[10], stime[11], stime[12]), col = 2, add = T)
            par(mfrow = c(1, 1))
            }
            s.hat <- ff2(t, stime)
            tss <- sum((c - mean(s))^2)
            rss <- sum((c - s.hat)^2)
            r.squared <- (tss - rss)/tss
            r.squared.adj <- 1 - ((1 - r.squared) * (length(s) - 
                1))/(length(s) - 1 - NROW(aa))
            list(Estimate = aa, Rsquared = r.squared, RsquaredAdj = r.squared.adj, 
                RSS = rss, residuals = resid, fitted=s.hat)
        }
        else if (shock == "mixed") {
            cat("--- Sorry you don't have 3 shocks in this case ---")
        }
    }
    else {
        cat("I'm sorry but we don't have implemented yet a model with more than 3 shocks.")
    }
}



###################################################################
#modifica della funzione GG.model
#cambio titoli figure, esporto previsioni, 
#aggiunto parametro ous per lunghezza out-of-sample



GG.model<-
function (sales, prelimestimates = NULL, mt = "base", alpha = 0.05, ous=200, display=T,
    ...) 
{
    x <- NULL
    t <- seq(1, length(sales), by = 1)
    s <- sales
    c <- cumsum(sales)
    if (is.function(mt) == T) {
        ff <- function(t, k, ps, qs) {
            k * mt(t) * (1 - exp(-(ps + qs) * t))/(1 + (qs/ps) * 
                exp(-(ps + qs) * t))
        }
        ff1 <- function(t, par) {
            c - ff(t, par[1], par[2], par[3])
        }
        ff2 <- function(t, par) {
            ff(t, par[1], par[2], par[3])
        }
        if (is.null(prelimestimates) == TRUE) {
            prelimestimates <- BASS.standard(sales = s, display = F)$Estimate[, 
                1]
        }
        stime <- nls.lm(par = prelimestimates, fn = ff1, t = t, 
            control = nls.lm.control(maxiter = 100))$par
        aa <- data.frame(summary(nls.lm(par = prelimestimates, 
            fn = ff1, t = t, control = nls.lm.control(maxiter = 100)))$coefficients[, 
            c(1, 2)], 0, 0, 0)
        names(aa) <- c("Estimate", "Std.Error", "Lower", "Upper", 
            "P-value")
        row.names(aa) <- c("k :", "ps :", "qs :")
        for (i in 1:NROW(aa)) {
            aa[i, c(3, 4)] <- aa[i, 1] + c(-1, 1) * qnorm(1 - 
                alpha/2) * aa[i, 2]
        }
        sssss <- signif(summary(nls.lm(par = prelimestimates, 
            fn = ff1, t = t, control = nls.lm.control(maxiter = 100)))$coefficients, 
            digits = 3)
        aa[, 5] <- sssss[, 4]
    }
    else {
        mt <- function(t, k, pc, qc) {
            k * sqrt((1 - exp(-(pc + qc) * t))/(1 + (qc/pc) * 
                exp(-(pc + qc) * t)))
        }
        ff0 <- function(t, k, pc, qc, ps, qs) {
            mt(t, k, pc, qc) * (1 - exp(-(ps + qs) * t))/(1 + 
                (qs/ps) * exp(-(ps + qs) * t))
        }
        ff1 <- function(t, par) {
            c - ff0(t, par[1], par[2], par[3], par[4], par[5])
        }
        ff2 <- function(t, par) {
            ff0(t, par[1], par[2], par[3], par[4], par[5])
        }
        if (is.null(prelimestimates) == TRUE) {
            prelimestimates <- c(BASS.standard(sales = s, display = F)$Estimate[, 
                1], 0.001, 0.1)
        }
        stime <- nls.lm(par = prelimestimates, fn = ff1, t = t, 
            control = nls.lm.control(maxiter = 100))$par
        aa <- data.frame(summary(nls.lm(par = prelimestimates, 
            fn = ff1, t = t, control = nls.lm.control(maxiter = 100)))$coefficients[, 
            c(1, 2)], 0, 0, 0)
        names(aa) <- c("Estimate", "Std.Error", "Lower", "Upper", 
            "P-value")
        row.names(aa) <- c("k :", "pc :", "qc :", "ps :", "qs :")
        for (i in 1:NROW(aa)) {
            aa[i, c(3, 4)] <- aa[i, 1] + c(-1, 1) * qnorm(1 - 
                alpha/2) * aa[i, 2]
        }
        sssss <- signif(summary(nls.lm(par = prelimestimates, 
            fn = ff1, t = t, control = nls.lm.control(maxiter = 100)))$coefficients, 
            digits = 3)
        aa[, 5] <- sssss[, 4]
    }
    if(display == TRUE){
    par(mfrow = c(1, 2))
    plot(t, c, main = "Cumulative", xlim = c(0, 
        max(t) + ous), ylim = c(0, ff2(max(t) + ous, stime)), ylab = "Cumulative")
    curve(ff2(x, stime), add = T, xlim = c(0, max(t) + ous), 
        ...)
    plot(t, sales, xlim = c(0, max(t) + ous), main = "Instantaneous", 
        ylab = "Instantaneous")
    curve(grad(function(t) ff2(t, stime), x, method = "simple"), 
        add = T, ...)
    par(mfrow = c(1, 1))
    }
    s.hat <- ff2(t, stime)
    tss <- sum((c - mean(s))^2)
    rss <- sum((c - s.hat)^2)
    r.squared <- 1 - rss/tss
    r.squared.adj <- 1 - ((1 - r.squared) * (length(s) - 1))/(length(s) - 
        1 - NROW(aa))
    res <- nls.lm(par = prelimestimates, fn = ff1, t = t, control = nls.lm.control(maxiter = 100))$fvec
    list(Estimate = aa, Rsquared = r.squared, RsquaredAdj = r.squared.adj, 
        residuals = res, fitted=s.hat)
}
