local did_init = false

function init()
  createGauge("hp_bar", 200, 20, 0,   0, "HP", "red"   )
  createGauge("mp_bar", 200, 20, 210, 0, "MP", "green" )
  createGauge("pp_bar", 200, 20, 420, 0, "PP", "blue"  )
  createGauge("fp_bar", 200, 20, 630, 0, "FP", "yellow")
  did_init = true
end

function curry_vitals()
  if not did_init then init() end
  setGauge("hp_bar", gmcp.Char.Vitals.curr_hp, gmcp.Char.Vitals.max_hp)
  setGauge("mp_bar", gmcp.Char.Vitals.curr_mp, gmcp.Char.Vitals.max_mp)
  setGauge("pp_bar", gmcp.Char.Vitals.curr_pp, gmcp.Char.Vitals.max_pp)
  setGauge("fp_bar", gmcp.Char.Vitals.curr_fp, gmcp.Char.Vitals.max_fp)
end
