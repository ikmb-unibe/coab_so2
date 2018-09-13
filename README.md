# COAB: Spill-over 2 / Agenda-setting paper

```
# links to data (not included in GitHub repository)
# just copy+paste code to load data set

# raw online data
data_online <- url("https://campuscloud.unibe.ch:443/ssf/s/readFile/share/24056/-1422573848356377275/publicLink/data_online.RData")
load(data_online)

# raw offline data
data_offline <- url("https://campuscloud.unibe.ch:443/ssf/s/readFile/share/24081/-6801514032582368813/publicLink/data_offline.RData")
load(data_offline)

# translated vocabulary (online)
vocab_translated <- url("https://campuscloud.unibe.ch:443/ssf/s/readFile/share/24082/-55354722181654842/publicLink/vocab_translated.RData")
load(vocab_translated)

# dfm with translated vocabulary (online)
dfm_en_translated <- url("https://campuscloud.unibe.ch:443/ssf/s/readFile/share/24083/2349918413178586565/publicLink/dfm_en_translated.RData")
load(dfm_en_translated)

# dfm online data
dfm_online <- url("https://campuscloud.unibe.ch:443/ssf/s/readFile/share/24117/8994127936163193836/publicLink/dfm_online.RData")
load(dfm_online)

# dfm offline data
dfm_offline <- url("https://campuscloud.unibe.ch:443/ssf/s/readFile/share/24118/7672670824778788549/publicLink/dfm_offline.RData")
load(dfm_offline)

# dfm combined
dfm_combined <- url("https://campuscloud.unibe.ch:443/ssf/s/readFile/share/24119/-2808791588121412080/publicLink/dfm_combined.RData")
load(dfm_combined)
```
