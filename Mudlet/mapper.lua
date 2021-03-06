mudlet = mudlet or {}; mudlet.mapper_script = true

local did_init = false
local yellow   = {255, 255, 0}

function curry_mapper()
  if not did_init then
    setMapZoom(10)
    did_init = true
  end
  local info = gmcp.Room.Info
  -- display(info)
  local area_id = find_area_id(info.area_name)
  if not area_id then area_id = addAreaName(info.area_name) end
  if not getRoomName(info.room_id) then
    addRoom(info.room_id)
    setRoomName(info.room_id, info.room_name)
    setRoomArea(info.room_id, area_id)
    setRoomCoordinates(info.room_id, info.x_coord, info.y_coord, info.z_coord)
    setRoomEnv(info.room_id, info.room_env)
    createMapLabel(area_id, info.room_label, info.x_coord, info.y_coord, info.z_coord, 255,255,255, 0,0,0, 0, 12)
    for i, dir in pairs(info.room_exits) do
      setExitStub(info.room_id, dir, true)
    end
  end
  if getRoomName(info.last_room_id) and info.last_room_id ~= info.room_id then
    if info.dir ~= -1 then
      setExit(info.last_room_id, info.room_id, info.dir)
      if info.dir == 11 or info.dir == 12 then -- "in" or "out"
        addCustomLine(info.last_room_id, info.room_id, info.dir, "solid line", yellow, false)
      end
    elseif info.special_dir ~= "-1" then
      addSpecialExit(info.last_room_id, info.room_id, info.special_dir)
      if addCustomLine ~= nil then
        addCustomLine(info.last_room_id, info.room_id, info.special_dir, "solid line", yellow, false)
      end
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
