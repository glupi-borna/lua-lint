-- luacheck:globals import linter string
-- luacheck:std lua51c
-- luacheck:ignore 131
-- luacheck:ignore 42./.*_
-- luacheck:ignore 43./.*_

local buffer = import("micro/buffer")
local micro = import("micro")
require "utils"

local helper_bufs = {}

function helper_buf(name, buf, bound_buf)
    local hp = {
        name = name,
        buf = buf,
        bound_buf = bound_buf
    }

    if helper_bufs[bound_buf.ID] == nil then
        helper_bufs[bound_buf.ID] = {[name] = hp}
    else
        helper_bufs[bound_buf.ID][name] = hp
    end

    return buf
end

function helper_open(name, dir, size, buf, bound_buf)
    if is_helper(bound_buf) then return end
    helper_close(bound_buf, name)

    if buf == nil then
        buf = buffer.NewBuffer("", name)
        helperize(buf)
    end

    local pane
    if dir == "h" then
        pane = micro.CurPane():HSplitIndex(buf, true)
        helper_buf(name, buf, bound_buf)
    elseif dir == "v" then
        pane = micro.CurPane():VSplitIndex(buf, true)
        helper_buf(name, buf, bound_buf)
    else
        info.set_error("Invalid pane direction: " .. dir .. "! Supported directions are h and v!")
        return
    end

    pane:ResizePane(size)
    pane:Relocate()

    return buf
end

function helperize(buf)
    buf.Type.Scratch = true
    buf.Type.Readonly = true
    buf:SetOptionNative("autosave", false)
    buf:SetOptionNative("diffgutter", false)
    buf:SetOptionNative("statusline", false)
end

function is_helper(buf)
    return helper_find(buf) ~= nil
end

function helper_debug()
    local out = ""
    for abspath, helpers in pairs(helper_bufs) do
        out = out .. abspath .. "\n"
        for name, _ in pairs(helpers) do
            out = out .. "\t" .. name .. "\n"
        end
    end
    micro.TermMessage(out)
end

function buffer_close_by_id(id)
    local panes = array.new(micro.FindPanesByBufferID(id))
    for _, pane in pairs(panes) do
        pane:Quit()
    end
end

function helper_close_all(bp)
    local helper = helper_find(bp.Buf)
    if helper ~= nil then
        buffer_close_by_id(helper.buf.ID)
        helper_bufs[helper.bound_buf.ID][helper.name] = nil
        return true
    end

    local helpers = helper_bufs[bp.Buf.ID]
    if helpers == nil then return false end

    for _, h in pairs(helpers) do
        buffer_close_by_id(h.buf.ID)
        helpers[h.name] = nil
    end
    return true
end

function helper_find(buf)
    for _, helpers in pairs(helper_bufs) do
        for _, helper in pairs(helpers) do
            if buf.ID == helper.buf.ID then return helper end
        end
    end
    return nil
end

function helper_close(buf, name)
    local found_buf = helper_find(buf)
    if found_buf ~= nil then
        buffer_close_by_id(found_buf.buf.ID)
        helper_bufs[found_buf.bound_buf.ID][found_buf.name] = nil
        return false
    end

    local helpers = helper_bufs[buf.ID]
    if helpers == nil then return false end

    local helper = helpers[name]
    if helper == nil then return false end
    buffer_close_by_id(helper.buf.ID)
    helpers[name] = nil
    return true
end

return {
    close = helper_close,
    close_all = helper_close_all,
    open = helper_open,
    is_helper = is_helper,
    helperize = helperize,
    debug = helper_debug
}
