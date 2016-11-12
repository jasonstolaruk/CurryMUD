did_init = false

function init()
  create_gauge("hp_bar", 200, 20, 0,   0, "HP", "red"   )
  create_gauge("mp_bar", 200, 20, 210, 0, "MP", "green" )
  create_gauge("pp_bar", 200, 20, 420, 0, "PP", "blue"  )
  create_gauge("fp_bar", 200, 20, 630, 0, "FP", "yellow")
  did_init = true
end

function curry_vitals()
  if not did_init then init() end
  set_gauge("hp_bar", gmcp.Char.Vitals.hp, gmcp.Char.Vitals.max_hp)
  set_gauge("mp_bar", gmcp.Char.Vitals.mp, gmcp.Char.Vitals.max_mp)
  set_gauge("pp_bar", gmcp.Char.Vitals.pp, gmcp.Char.Vitals.max_pp)
  set_gauge("fp_bar", gmcp.Char.Vitals.fp, gmcp.Char.Vitals.max_fp)
end
