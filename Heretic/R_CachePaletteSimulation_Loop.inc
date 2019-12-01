          c := plw^;
          {$IFDEF TRANSPARENCYNEEDED}
          if c <> 0 then // JVAL: color $000000 is transparent index
          begin
          {$ENDIF}
            r2 := c;
            g2 := c shr 8;
            b2 := c shr 16;
            r := r1 + r2;
            if r > 255 then
              r := 255
            {$IFDEF USEGAMMA}
            else
              r := curgamma[r];
            {$ELSE};{$ENDIF}
            g := g1 + g2;
            if g > 255 then
              g := 255
            {$IFDEF USEGAMMA}
            else
              g := curgamma[g];
            {$ELSE};{$ENDIF}
            b := b1 + b2;
            if b > 255 then
              plw^ := r + g shl 8 + $FF0000
            else
            {$IFDEF USEGAMMA}
              plw^ := r + g shl 8 + curgamma[b] shl 16;
            {$ELSE}
              plw^ := r + g shl 8 + b shl 16;
            {$ENDIF}
          {$IFDEF TRANSPARENCYNEEDED}
          end;
          {$ENDIF}
          inc(plw);

