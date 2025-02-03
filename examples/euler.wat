(module
  (type (;0;) (func (result i32)))
  (export "main" (func 0))
  (func (;0;) (type 0) (result i32)
    (local i32 i32 i32)
    i32.const 0
    local.set 0
    i32.const 1
    local.set 1
    loop ;; label = @1
      local.get 1
      i32.const 10
      i32.lt_s
      if ;; label = @2
        local.get 1
        i32.const 5
        i32.rem_s
        i32.const 0
        i32.eq
        if (result i32) ;; label = @3
          local.get 1
        else
          i32.const 0
        end
        local.set 2
        local.get 0
        local.get 2
        i32.add
        local.tee 0
        drop
        local.get 1
        i32.const 1
        i32.add
        local.tee 1
        drop
        br 1 (;@1;)
      end
    end
    local.get 0
  )
)
