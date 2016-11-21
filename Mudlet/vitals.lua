local did_init = false
local hp_bar, mp_bar, pp_bar, fp_bar

function init()
  did_init = true

  setBorderTop(2)
  setBorderBottom(26)
  setBorderLeft(2)

  gauge_container = Geyser.Container:new({
    name = "gauge_container",
    x = 0, y = -26,
    width = "100%", height = 26,
  })

  hp_bar = Geyser.Gauge:new({
    name = "hp_bar",
    x = 2, y = 2,
    width = 200, height = 20,
  }, gauge_container)

  mp_bar = Geyser.Gauge:new({
    name = "mp_bar",
    x = 207, y = 2,
    width = 200, height = 20,
  }, gauge_container)

  pp_bar = Geyser.Gauge:new({
    name = "pp_bar",
    x = 412, y = 2,
    width = 200, height = 20,
  }, gauge_container)

  fp_bar = Geyser.Gauge:new({
    name = "fp_bar",
    x = 617, y = 2,
    width = 200, height = 20,
  }, gauge_container)

  hp_bar.front:setStyleSheet([[background-color: red]])
  hp_bar.back:setStyleSheet([[background-color: #808080]])
  mp_bar.front:setStyleSheet([[background-color: green]])
  mp_bar.back:setStyleSheet([[background-color: #808080]])
  pp_bar.front:setStyleSheet([[background-color: blue]])
  pp_bar.back:setStyleSheet([[background-color: #808080]])
  fp_bar.front:setStyleSheet([[background-color: yellow]])
  fp_bar.back:setStyleSheet([[background-color: #cccccc]])
end

function curry_vitals()
  if not did_init then init() end

  local hp_text = "&nbsp;<b><font size=\"3\">HP: "..gmcp.Char.Vitals.curr_hp.."</font></b>"
  local mp_text = "&nbsp;<b><font size=\"3\">MP: "..gmcp.Char.Vitals.curr_mp.."</font></b>"
  local pp_text = "&nbsp;<b><font size=\"3\">PP: "..gmcp.Char.Vitals.curr_pp.."</font></b>"
  local fp_text = "&nbsp;<b><font size=\"3\" color=\"black\">FP: "..gmcp.Char.Vitals.curr_fp.."</font></b>"
  hp_bar:setValue(gmcp.Char.Vitals.curr_hp, gmcp.Char.Vitals.max_hp, hp_text)
  mp_bar:setValue(gmcp.Char.Vitals.curr_mp, gmcp.Char.Vitals.max_mp, mp_text)
  pp_bar:setValue(gmcp.Char.Vitals.curr_pp, gmcp.Char.Vitals.max_pp, pp_text)
  fp_bar:setValue(gmcp.Char.Vitals.curr_fp, gmcp.Char.Vitals.max_fp, fp_text)
end
