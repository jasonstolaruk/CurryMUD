mudlet = mudlet or {}; mudlet.mapper_script = true

local did_init = false

function init()
  did_init = true
  setMapZoom(12)
  local r, g, b
  -- Inside
  r, g, b = unpack(color_table.brown)
  setCustomEnvColor(200, r, g, b, 255)
  -- Outside
  r, g, b = unpack(color_table.green)
  setCustomEnvColor(201, r, g, b, 255)
  -- Shop
  r, g, b = unpack(color_table.blue)
  setCustomEnvColor(202, r, g, b, 255)
  -- Special
  r, g, b = unpack(color_table.yellow)
  setCustomEnvColor(203, r, g, b, 255)
  -- None
  r, g, b = unpack(color_table.white)
  setCustomEnvColor(204, r, g, b, 255)
end

function curry_mapper()
  if not did_init then init() end
  local info = gmcp.Room.Info
  -- display(info)
  local area_id = find_area_id(info.area_name)
  if not area_id then area_id = addAreaName(info.area_name) end
  if not find_room_id(info.room_id) then
    addRoom(info.room_id)
    setRoomName(info.room_id, info.room_name)
    setRoomArea(info.room_id, area_id)
    setRoomCoordinates(info.room_id, info.x_coord, info.y_coord, info.z_coord)
    setRoomEnv(info.room_id, info.room_env)
    createMapLabel(area_id, info.room_label, info.x_coord, info.y_coord, info.z_coord, 255,255,255, 0,0,0, 0, 14)
  end
  if find_room_id(info.last_room_id) and info.last_room_id ~= info.room_id then
    if info.dir ~= -1 then
      setExit(info.last_room_id, info.room_id, info.dir)
    elseif info.special_dir ~= "-1" then
      addSpecialExit(info.last_room_id, info.room_id, info.special_dir)
      addCustomLine(info.last_room_id, info.room_id, info.special_dir, "solid line", {0, 255, 255}, true)
    end
  end
  centerview(info.room_id)
end

function find_area_id(name)
  for area, id in pairs(getAreaTable()) do
    if area:find(name, 1, true) then return id end
  end
end

function find_room_id(room_id)
  for id in pairs(getRooms()) do
    if id == room_id then return id end
  end
end
