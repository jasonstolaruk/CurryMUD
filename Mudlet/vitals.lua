did_init = false

function init()
  createGauge("hp_bar", 200, 20, 0,   0, "HP", "red"   )
  createGauge("mp_bar", 200, 20, 210, 0, "MP", "green" )
  createGauge("pp_bar", 200, 20, 420, 0, "PP", "blue"  )
  createGauge("fp_bar", 200, 20, 630, 0, "FP", "yellow")
  did_init = true
end

function curry_vitals()
  if not did_init then init() end
  set_gauge("hp_bar", gmcp.Char.Vitals.curr_hp, gmcp.Char.Vitals.max_hp)
  set_gauge("mp_bar", gmcp.Char.Vitals.curr_mp, gmcp.Char.Vitals.max_mp)
  set_gauge("pp_bar", gmcp.Char.Vitals.curr_pp, gmcp.Char.Vitals.max_pp)
  set_gauge("fp_bar", gmcp.Char.Vitals.curr_fp, gmcp.Char.Vitals.max_fp)
end
