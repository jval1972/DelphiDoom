        c2 := vplj^;

        // Color averaging
        r2 := c2;
        g2 := c2 shr 8;
        b2 := c2 shr 16;

        r := (cf2[r2] + rA) shr FRACBITS;
        g := (cf2[g2] + gA) shr FRACBITS;
        b := (cf2[b2] + bA) and $FF0000;

        hrtp^ := r + g shl 8 + b;

        inc(hrtp);
{$IFNDEF LASTLOOP}
        inc(vplj);
{$ENDIF}

