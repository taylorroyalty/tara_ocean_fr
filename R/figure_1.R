library(tidyverse)
library(ggridges)
df.t=read.csv('data/raw/metaT_genus_rank.csv')
df.g=read.csv('data/raw/metaG_rank.csv')

#################################################
#SULFUR
#assimilatory sulfate reduction
ASR<-c("K13811","K00958","K00860","K00955","K00957","K00956","K00860","K00390","K00380","K00381","K00392")
#dissimilatory sulfate reduction
DSR<-c("K00958","K00394","K00395","K11180","K11181")
#thiosulfate oxidation
TSO<-c("K17222","K17223","K17224","K17225","K22622","K17226","K17227")

sulfur_list<-list(ASR,DSR,TSO)
names(sulfur_list)<-c("ASR","DSR","TSO")
#################################################

#################################################
#Photosynthesis
#photosystem I
photo_1<-c("K02703","K02706","K02705","K02704","K02707","K02708")
#photosystem II
photo_2<-c("K02689","K02690","K02691","K02692","K02693","K02694")
#	Cytochrome b6f complex
cytobf<-c("K02635","K02637","K02634","K02636","K02642","K02643","K03689","K02640")
#	F-type ATPase, prokaryotes and chloroplasts
ftype<-c("K02111","K02112","K02113","K02114","K02115","K02108","K02109","K02110")

photo_list<-list(photo_1,photo_2,cytobf,ftype)
names(photo_list)<-c("photo_1","photo_2","cytobf","ftype")
#################################################

#################################################
#Nitrogen
#nitrogen fixation
NF<-c("K02588","K02586","K02591","K00531","K22896","K22897","K22898","K22899")
#Assimilatory nitrate reduction
ANR<-c("K00367","K10534","K00372","K00360","K00366","K17877")
#Dissimlatory nitrate reduction
DNR<-c("K00370","K00371","K00374","K02567","K02568","K00362","K00363","K03385","K15876")
#denitrification
denitr<-c("K00370","K00371","K00374","K02567","K02568","K00368","K15864","K04561","K02305","K00376")
#nitrification
nitr<-c("K10944","K10945","K10946","K10535")
#complete nitrificaiton
cnitr<-c("K10944","K10945","K10946","K10535","K00370","K00371")

nitrogen_list<-list(cnitr,nitr,denitr,DNR,ANR,NF)
names(nitrogen_list)<-c("cnitr","nitr","denitr","DNR","ANR","NF")
#################################################

#################################################
#Methane
#Methanogensis_CO2
methano_CO2<-c("K00200","K00201","K00202","K00203","K11261","K00205","K11260","K00204","K00672","K01499","K00319","K13942","K00320","K00577","K00578","K00579","K00580",
               "K00581","K00582","K00583","K00584","K00399","K00401","K00402","K22480","K22481","K22482","K03388","K03389","K03390","K08264","K08265","K03388",
               "K03389","K03390","K14127","K14126","K14128","K00125")
#Methanogensis_acetate
methano_ace<-c("K00925","K00625","K01895","K00193","K00197","K00194","K00577","K00578","K00579","K00580","K00581","K00582","K00583","K00584","K00399","K00401","K00402",
               "K22480","K22481","K22482","K03388","K03389","K03390","K08264","K08265","K03388","K03389","K03390","K14127","K14126","K14128","K22516","K00125")
#Methanogensis_methanol
methano_CH3_OH<-c("K14080","K04480","K14081","K00399","K00401","K00402","K22480","K22481","K22482","K03388","K03389","K03390","K08264","K08265","K03388","K03389",
                  "K03390","K14127","K14126","K14128","K22516","K00125")
#methanogenesis_methylamine
methano_methyl<-c("K14082","K16177","K16176","K16179","K16178","K14084","K14083","K00399","K00401","K00402","K22480","K22481","K22482","K03388","K03389","K03390",
                  "K08264","K08265","K03388","K03389","K03390","K14127","K14126","K14128","K22516","K00125")
#coenzyme m biosynthesis
coenzyme_m<-c("K08097","K05979","K05884","K13039","K06034")
#	2-Oxocarboxylic acid chain extension, 2-oxoglutarate => 2-oxoadipate => 2-oxopimelate => 2-oxosuberate
oxocarb<-c("K10977","K16792","K16793","K10978")
#Methane oxidation
meth_ox<-c("K10944","K10945","K10946","K16157","K16158","K16159","K16160","K16161","K16162","K14028","K14029","K23995")
#Formaldehyde assimiation
form_serine<-c("K00600","K00830","K00018","K11529","K01689","K01595","K00024","K08692","K14067","K08691")
#formaldehyde ribulose monophosphate
form_ribu<-c("K08093","K13812","K08094","K13831","K00850","K16370","K01624")
#formaldehyde xylulose monophosphate pathway
form_xyl<-c("K17100","K00863","K01624","K03841")
#f420 biosynthesis
f420<-c("K11779","K11780","K11781","K14941","K11212","K12234")
#methanofuran biosynthesis
meth_furan<-c("K09733","K19793","K07144","K18933","K06914","K07072")
#acetyl-coa
coA<-c("K00192","K00195","K00193","K00197","K00194")

methane_list=list(methano_CO2,methano_ace,methano_CH3_OH,methano_methyl,coenzyme_m,oxocarb,meth_ox,form_serine,form_ribu,form_xyl,f420,meth_furan,coA)
names(methane_list)<-c("methano_CO2","methano_ace","methano_CH3_OH","methano_methyl","coenzyme_m","oxocarb","meth_ox","form_serine","form_ribu","form_xyl","f420","meth_furan","coA")
#################################################

#################################################
#Carbon Fixation
#reductive citrate cycle
RCC<-c("K00169","K00170","K00171","K00172","K03737","K01007","K01006","K01595","K01959","K01960","K01958","K00024","K01676","K01679","K01677","K01678","K00239","K00240","K00241",
       "K00242","K00244","K00245","K00246","K00247","K18556","K18557","K18558","K18559","K18560","K01902","K01903","K00174","K00175","K00177","K00176",
       "K00031","K01681","K01682","K15230","K15231","K15232","K15233","K15234")
#3-Hydroxypropionate bi-cycle
hydroxprop_bi<-c("K02160","K01961","K01962","K01963","K14468","K14469","K15052","K05606","K01847","K01848","K01849","K14471","K14472","K00239","K00240","K00241",
                 "K01679","K08691","K14449","K14470","K09709")
#	Hydroxypropionate-hydroxybutylate cycle
hydroxprop_hyr<-c("K01964","K15037","K15036","K15017","K15039","K15018","K15019","K15020","K05606","K01848","K01849","K15038","K15017","K14465",
                  "K14466","K18861","K14534","K15016","K00626")
#Dicarboxylate-hydroxybutyrate cycle
dicarbox_hyr<-c("K00169","K00170","K00171","K00172","K01007","K01595","K00024","K01677","K01678","K00239","K00240","K00241","K18860","K01902","K01903","K15038",
                "K15017","K14465","K14467","K18861","K14534","K15016","K00626")
#Reductive acetyl-CoA pathway (Wood-Ljungdahl pathway)
reduc_acetyl<-c("K00198","K05299","K15022","K01938","K01491","K00297","K15023","K14138","K00197","K00194")

#	Phosphate acetyltransferase-acetate kinase pathway, acetyl-CoA => acetate
phos_acetyl<-c("K00625","K13788","K15024","K00925")
#	Incomplete reductive citrate cycle, acetyl-CoA => oxoglutarate
incomp<-c("K00169","K00170","K00171","K00172","K01959","K01960","K00024","K01677","K01678","K18209","K18210","K01902","K01903","K00174","K00175","K00176","K00177")

fixation_list<-list(RCC,hydroxprop_bi,hydroxprop_hyr,dicarbox_hyr,reduc_acetyl,phos_acetyl,incomp)
names(fixation_list)<-c("RCC","hydroxprop_bi","hydroxprop_hyr","dicarbox_hyr","reduc_acetyl","phos_acetyl","incomp")
#################################################

#################################################
#Oxidative Phosphorylation
#	NADH:quinone oxidoreductase, prokaryotes
quin_oxio<-c("K00330","K00331","K00332","K00333","K00331","K13378","K13380","K00334","K00335","K00336","K00337","K00338","K00339","K00340","K00341","K00342","K00343")
#	NAD(P)H:quinone oxidoreductase, chloroplasts and cyanobacteria
quin_cyan<-c("K05574","K05582","K05581","K05579","K05572","K05580","K05578","K05576","K05577","K05575","K05573","K05583","K05584","K05585")
#NADH:ubiquinone oxidoreductase, mitochondria
quin_mito<-c("K03878","K03879","K03880","K03881","K03882","K03883","K03884")
#	NADH dehydrogenase (ubiquinone) Fe-S protein/flavoprotein complex, mitochondria
quino_FS<-c("K03934","K03935","K03936","K03937","K03938","K03939","K03940","K03941","K03942","K03943","K03944")
#	NADH dehydrogenase (ubiquinone) 1 alpha subcomplex
alpha_1<-c("K03945","K03946","K03947","K03948","K03949","K03950","K03951","K03952","K03953","K03954","K03955","K03956","K11352","K11353")
#NADH dehydrogenase (ubiquinone) 1 beta subcomplex
beta_1<-c("K03957","K03958","K03959","K03960","K03961","K03962","K03963","K03964","K03965","K03966","K11351","K03967","K03968")
#succinate dehydro
succinate_dehydro<-c("K00241","K00242","K18859","K18860","K00239","K00240")
#Fumarate reductase, prokaryotes
fum_red<-c("K00244","K00245","K00246","K00247")
#Succinate dehydrogenase (ubiquinone)
succinate_dehydro.u<-c("K00236","K00237","K00234","K00235")
# Cytochrome bc1 complex respiratory unit
cytobc1.resp<-c("K00412","K00413","K00410","K00411","K03886","K03887","K03888","K03890","K03891","K03889")
#	Cytochrome bc1 complex
cytobc1<-c("K00411","K00412","K00413","K00414","K00415","K00416","K00417","K00418","K00419","K00420")
#	Cytochrome c oxidase
cytoc<-c("K02257","K02262","K02256","K02261","K02263","K02264","K02265","K02266","K02267","K02268","K02270","K02271","K02272","K02273","K02258","K02259","K02260")
#Cytochrome c oxidase, prokaryotes
cytoc_pro<-c("K02275","K02274","K02276","K15408","K02277")
#Cytochrome bd ubiquinol oxidase
cytobd<-c("K00425","K00426","K00424","K22501")
#Cytochrome o ubiquinol oxidase
cytoo<-c("K02297","K02298","K02299","K02300")
#	Cytochrome aa3-600 menaquinol oxidase
cytoaa<-c("K02827","K02826","K02828","K02829")
#	Cytochrome c oxidase, cbb3-type
cytoc_cbb3<-c("K00404","K00405","K15862","K00407","K00406")
#	F-type ATPase, prokaryotes and chloroplasts
ftype_pro<-c("K02111","K02112","K02113","K02114","K02115","K02108","K02109","K02110")
#	F-type ATPase, eukaryotes
ftype_eu<-c("K02132","K02133","K02136","K02134","K02135","K02137","K02126","K02127","K02128","K02138","K02129","K01549","K02130","K02139","K02140","K02141","K02131",
            "K02142","K02143","K02125")
#V/A-type ATPase, prokaryotes
vatype_pro<-c("K02117","K02118","K02119","K02120","K02121","K02122","K02107","K02123","K02124")
#	V-type ATPase, eukaryotes
vatype_eu<-c("K02145","K02147","K02148","K02149","K02150","K02151","K02152","K02144","K02154","K03661","K02155","K02146","K02153","K03662")

ox_phos_list<-list(quin_oxio,quin_cyan,quin_mito,quino_FS,alpha_1,beta_1,succinate_dehydro,fum_red,succinate_dehydro.u,cytobc1.resp,cytobc1,cytoc,cytoc_pro,cytobd,cytoo,cytoaa,cytoc_cbb3,ftype_pro,ftype_eu,vatype_pro,vatype_eu)
names(ox_phos_list)<-c("quin_oxio","quin_cyan","quin_mito","quino_FS","alpha_1","beta_1","succinate_dehydro","fum_red",'succinate_dehydro.u',"cytobc1.resp","cytobc1","cytoc","cytoc_pro","cytobd","cytoo","cytoaa","cytoc_cbb3","ftype_pro","ftype_eu","vatype_pro","vatype_eu")
#################################################

df.metabolism<-data.frame(KO=unlist(ox_phos_list),anno="Oxidative Phosphorylation") %>%
  rbind(data.frame(KO=unlist(photo_list),anno="Photosynthesis")) %>%
  rbind(data.frame(KO=unlist(sulfur_list),anno="Sulfur Metabolism")) %>%
  rbind(data.frame(KO=unlist(nitrogen_list),anno="Nitrogen Metabolism")) %>%
  rbind(data.frame(KO=unlist(methane_list),anno="Methane Metabolism")) %>%
  rbind(data.frame(KO=unlist(fixation_list),anno="Carbon Fixation"))# %>%


df_density<-df.t %>%
  filter(KO %in% df.t$KO) %>%
  merge(df.metabolism,by='KO') %>%
  dplyr::select(fr,anno) %>%
  rbind(df.t %>%
          dplyr::select(fr) %>%
          data.frame(anno='all')) %>%
  cbind(type="transcript") %>%
  rbind(df.g %>%
          filter(KO %in% df.g$KO) %>%
          merge(df.metabolism,by='KO') %>%
          dplyr::select(fr,anno) %>%
          rbind(df.g %>%
                  dplyr::select(fr) %>%
                  data.frame(anno='all')) %>%
          cbind(data.frame(type="gene")))


#Figure 1A and B
#Everything was offset by 1e-5 to plot on log-scale. Mode corresponding to fr==0 was graphically edited via illustrator
ggplot(data=df_density,aes(x=fr+1e-5,y=anno,alpha=0.3))+
  geom_density_ridges(scale = 2, rel_min_height = 0.01, quantile_lines = TRUE, quantiles = c(0.025,0.5, 0.975)) +
  scale_x_log10(limits=c(1e-6,1)) + facet_wrap(.~type)

colnames(df.t)[1]<-"fr.t"
colnames(df.g)[1]<-"fr.g"
df.tg<-merge(df.g %>% 
               dplyr::select(fr.g,site,KO),
             df.t %>% 
               dplyr::select(fr.t,site,KO),by=c("KO","site"))

#offset for regression modeling after log-transformation
df.tg$fr.g<-log10(df.tg$fr.g+1e-4)
df.tg$fr.t<-log10(df.tg$fr.t+1e-4)

#Filter high leverage data
lm1<-lm(fr.t~fr.g,df.tg)
h.lev<-cooks.distance(lm1) < 4/nrow(df.tg)

lm2<-lm(fr.t~fr.g,df.tg[h.lev,])
summary(lm2)

#Figure 1C
ggplot(df.tg[h.lev,])+stat_density_2d(geom='raster',contour=FALSE,aes(fr.g,fr.t,fill=after_stat(log10(density))),n=200)+
  scale_fill_distiller(palette = "RdYlBu",limits=c(-2,1),na.value = 'transparent') +
  xlim(-3,0) +
  ylim(-3,0) +
  geom_smooth(aes(fr.g,fr.t),method = 'lm',color='black')
