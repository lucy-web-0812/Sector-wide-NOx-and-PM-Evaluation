# And now looking at the PM plot too 


ggplot(collated_data_pm) +
  geom_point(aes(x=`Power (kW)`, y = `PM (mg/kWh)`, colour = Directive)) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10")
