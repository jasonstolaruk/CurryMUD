mudlet = mudlet or {}; mudlet.mapper_script = true

local did_init = false

function curry_mapper()
  if not did_init then
    setMapZoom(14)
    did_init = true
  end
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
    createMapLabel(area_id, info.room_label, info.x_coord, info.y_coord, info.z_coord, 255,255,255, 0,0,0, 0, 12)
  end
  if find_room_id(info.last_room_id) and info.last_room_id ~= info.room_id then
    if info.dir ~= -1 then
      setExit(info.last_room_id, info.room_id, info.dir)
    elseif info.special_dir ~= "-1" then
      addSpecialExit(info.last_room_id, info.room_id, info.special_dir)
      addCustomLine(info.last_room_id, info.room_id, info.special_dir, "solid line", {255, 0, 255}, false)
    end
  end
  if info.zoom ~= -1 then setMapZoom(info.zoom) end
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
