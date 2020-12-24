    case ds_size of
    FS64x64:
      begin
        if ds_scale = ds64x64 then
        begin
        {$DEFINE SCALE64x64}
        {$UNDEF SCALE128x128}
        {$UNDEF SCALE256x256}
        {$UNDEF SCALE512x512}
        {$I R_DrawSpanNormal_Main.inc}
        end
        else if ds_scale = ds128x128 then
        begin
        {$UNDEF SCALE64x64}
        {$DEFINE SCALE128x128}
        {$UNDEF SCALE256x256}
        {$UNDEF SCALE512x512}
        {$I R_DrawSpanNormal_Main.inc}
        end
        else if ds_scale = ds256x256 then
        begin
        {$UNDEF SCALE64x64}
        {$UNDEF SCALE128x128}
        {$DEFINE SCALE256x256}
        {$UNDEF SCALE512x512}
        {$I R_DrawSpanNormal_Main.inc}
        end
        else
        begin
        {$UNDEF SCALE64x64}
        {$UNDEF SCALE128x128}
        {$UNDEF SCALE256x256}
        {$DEFINE SCALE512x512}
        {$I R_DrawSpanNormal_Main.inc}
        end;
      end;
    FS128x128:
      begin
        {$UNDEF SCALE64x64}
        {$UNDEF SCALE128x128}
        {$UNDEF SCALE256x256}
        {$UNDEF SCALE512x512}
        {$UNDEF SIZE64x64}
        {$DEFINE SIZE128x128}
        {$UNDEF SIZE256x256}
        {$UNDEF SIZE512x512}
        {$UNDEF SIZE1024x1024}
        {$UNDEF SIZE2048x2048}
        {$UNDEF SIZE4096x4096}
        {$I R_DrawSpanNormalSize_Main.inc}
      end;
    FS256x256:
      begin
        {$UNDEF SCALE64x64}
        {$UNDEF SCALE128x128}
        {$UNDEF SCALE256x256}
        {$UNDEF SCALE512x512}
        {$UNDEF SIZE64x64}
        {$UNDEF SIZE128x128}
        {$DEFINE SIZE256x256}
        {$UNDEF SIZE512x512}
        {$UNDEF SIZE1024x1024}
        {$UNDEF SIZE2048x2048}
        {$UNDEF SIZE4096x4096}
        {$I R_DrawSpanNormalSize_Main.inc}
      end;
    FS512x512:
      begin
        {$UNDEF SCALE64x64}
        {$UNDEF SCALE128x128}
        {$UNDEF SCALE256x256}
        {$UNDEF SCALE512x512}
        {$UNDEF SIZE64x64}
        {$UNDEF SIZE128x128}
        {$UNDEF SIZE256x256}
        {$DEFINE SIZE512x512}
        {$UNDEF SIZE1024x1024}
        {$UNDEF SIZE2048x2048}
        {$UNDEF SIZE4096x4096}
        {$I R_DrawSpanNormalSize_Main.inc}
      end;
    FS1024x1024:
      begin
        {$UNDEF SCALE64x64}
        {$UNDEF SCALE128x128}
        {$UNDEF SCALE256x256}
        {$UNDEF SCALE512x512}
        {$UNDEF SIZE64x64}
        {$UNDEF SIZE128x128}
        {$UNDEF SIZE256x256}
        {$UNDEF SIZE512x512}
        {$DEFINE SIZE1024x1024}
        {$UNDEF SIZE2048x2048}
        {$UNDEF SIZE4096x4096}
        {$I R_DrawSpanNormalSize_Main.inc}
      end;
    FS2048x2048:
      begin
        {$UNDEF SCALE64x64}
        {$UNDEF SCALE128x128}
        {$UNDEF SCALE256x256}
        {$UNDEF SCALE512x512}
        {$UNDEF SIZE64x64}
        {$UNDEF SIZE128x128}
        {$UNDEF SIZE256x256}
        {$UNDEF SIZE512x512}
        {$UNDEF SIZE1024x1024}
        {$DEFINE SIZE2048x2048}
        {$UNDEF SIZE4096x4096}
        {$I R_DrawSpanNormalSize_Main.inc}
      end;
    FS4096x4096:
      begin
        {$UNDEF SCALE64x64}
        {$UNDEF SCALE128x128}
        {$UNDEF SCALE256x256}
        {$UNDEF SCALE512x512}
        {$UNDEF SIZE64x64}
        {$UNDEF SIZE128x128}
        {$UNDEF SIZE256x256}
        {$UNDEF SIZE512x512}
        {$UNDEF SIZE1024x1024}
        {$UNDEF SIZE2048x2048}
        {$DEFINE SIZE4096x4096}
        {$I R_DrawSpanNormalSize_Main.inc}
      end;
    end;
