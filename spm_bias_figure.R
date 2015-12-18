df = read.csv('../../data/brain_volumes.csv',row.names = 1)

s_fs = ggplot(data = df,aes(y = TIV_SPM,x = TIV_FS))+
  geom_point(aes(col = factor(control)),size = 1.4)+
  geom_abline(slope = 1,linetype = "dotted",size = 1.1)+
  geom_smooth(method = "lm",aes(col = factor(control)),se = FALSE,size = 1.2)+ 
  scale_color_manual(values = c("#D55E00", "#009E73"),name = "",guide = FALSE)+
  theme_bw(base_size = 50)+coord_cartesian(ylim  =  c(0.9, 2.1))

s_fsl = ggplot(data = df,aes(y = TIV_SPM,x = TIV_FSL))+
  geom_point(aes(col = factor(control)),size = 1.4)+
  geom_abline(slope = 1,linetype = "dotted",size = 1.1)+
  geom_smooth(method = "lm",aes(col = factor(control)),se = FALSE,size = 1.2)+  
  theme_bw(base_size = 50)+
  theme(legend.position  =  c(0.5, 0.9),legend.key.size  =  unit(1.5, "cm"))+scale_colour_manual(name = "",labels = c("ASD","TDC"),breaks = c(0,1),values = c("#D55E00", "#009E73"))+
  coord_cartesian(ylim  =  c(0.9, 2.1))

fsl_fs = ggplot(data = df,aes(y = TIV_FSL,x = TIV_FS))+
  geom_point(aes(col = factor(control)),size = 1.4)+
  geom_abline(slope = 1,linetype = "dotted",size = 1.1)+
  geom_smooth(method = "lm",aes(col = factor(control)),se = FALSE,size = 1.2)+ 
  scale_color_manual(values = c("#D55E00", "#009E73"),name = "",guide = FALSE)+
  theme_bw(base_size = 50)+
  coord_cartesian(ylim  =  c(0.9, 2.1))

pdf(file = ('spm_bias.pdf'),height = 10,width = 30)

grid.arrange(s_fs,s_fsl,fsl_fs,ncol = 3)

dev.off()
