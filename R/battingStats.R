# Metrics not in current Lahman. There is no "pitchingStats table in R Lahman"

# wOBA

# k_pct and bb_pct
# b$Kpct = round((b$SO/b$PA), 3) #Strikeout Rate 
#b$BBpct = round((b$BB/b$PA), 3) #Base on Balls rate

# hr_pct
# xbh_pct (extra base hit percentage)
# (2b + 3b +hr / pa)
# x_h_pct (extra base per hit)
# (2b + 3b +hr / h)

# Contact Rate
# b$ContactRate = round(((b$AB-b$SO)/b$AB), 3) #Batter contact rate

# Total bases
# h + 2*2b + 3*3b + 4*hr

# RC

# bip_pct (balls in play percentage)
# (ab - so - hr + sf) / pa

# http://www.fangraphs.com/library/offense/wrc/
# wRC (weighted runs created)
# wRC = (((wOBA-League wOBA)/wOBA Scale)+(League R/PA))*PA


# wRC+ this one uses park factors and leauge adjustments.
# wRC+ = (((wRAA/PA + League R/PA) + (League R/PA – Park Factor* League R/PA))/ 
# (AL or NL wRC/PA excluding pitchers))*100

# RAA (runs above average)

# WAA (wins above average)

# wRAA (weighted runs above average)
# http://www.fangraphs.com/library/offense/wraa/
# wRAA = ((wOBA – league wOBA) / wOBA scale) × PA

# ISO
# iso <- ((X2B + (2 * X3B) + (3 * HR) / AB), 3)

# OPS+
# Baseball refreence has its own method. Try to find a couple.

# OFF (offensive runs above average)
# http://www.fangraphs.com/library/offense/off/