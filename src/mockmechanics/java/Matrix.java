package mockmechanics.java;

public class Matrix {
    public static void setRotateM(float[] rm, float a, float x, float y, float z) {
        rm[3] = 0;
        rm[7] = 0;
        rm[11]= 0;
        rm[12]= 0;
        rm[13]= 0;
        rm[14]= 0;
        rm[15]= 1;
        a *= (float) (Math.PI / 180.0f);
        float s = (float) Math.sin(a);
        float c = (float) Math.cos(a);
        if (1.0f == x && 0.0f == y && 0.0f == z) {
            rm[5] = c;   rm[10]= c;
            rm[6] = s;   rm[9] = -s;
            rm[1] = 0;   rm[2] = 0;
            rm[4] = 0;   rm[8] = 0;
            rm[0] = 1;
        } else if (0.0f == x && 1.0f == y && 0.0f == z) {
            rm[0] = c;   rm[10]= c;
            rm[8] = s;   rm[2] = -s;
            rm[1] = 0;   rm[4] = 0;
            rm[6] = 0;   rm[9] = 0;
            rm[5] = 1;
        } else if (0.0f == x && 0.0f == y && 1.0f == z) {
            rm[0] = c;   rm[5] = c;
            rm[1] = s;   rm[4] = -s;
            rm[2] = 0;   rm[6] = 0;
            rm[8] = 0;   rm[9] = 0;
            rm[10]= 1;
        } else {
            float len = length(x, y, z);
            if (1.0f != len) {
                float recipLen = 1.0f / len;
                x *= recipLen;
                y *= recipLen;
                z *= recipLen;
            }
            float nc = 1.0f - c;
            float xy = x * y;
            float yz = y * z;
            float zx = z * x;
            float xs = x * s;
            float ys = y * s;
            float zs = z * s;
            rm[0] = x*x*nc +  c;
            rm[4] =  xy*nc - zs;
            rm[8] =  zx*nc + ys;
            rm[1] =  xy*nc + zs;
            rm[5] = y*y*nc +  c;
            rm[9] =  yz*nc - xs;
            rm[2] =  zx*nc - ys;
            rm[6] =  yz*nc + xs;
            rm[10] = z*z*nc +  c;
        }
    }

    public static void translateM(float[] m,
                                  float x, float y, float z) {
        for (int i=0 ; i<4 ; i++) {
            int mi = i;
            m[12 + mi] += m[mi] * x + m[4 + mi] * y + m[8 + mi] * z;
        }
    }

    public static float length(float x, float y, float z) {
        return (float) Math.sqrt(x * x + y * y + z * z);
    }

    public static void setLookAtM(float[] rm,
                                  float eyeX, float eyeY, float eyeZ,
                                  float centerX, float centerY, float centerZ, float upX, float upY,
                                  float upZ) {

        // See the OpenGL GLUT documentation for gluLookAt for a description
        // of the algorithm. We implement it in a straightforward way:

        float fx = centerX - eyeX;
        float fy = centerY - eyeY;
        float fz = centerZ - eyeZ;

        // Normalize f
        float rlf = 1.0f / length(fx, fy, fz);
        fx *= rlf;
        fy *= rlf;
        fz *= rlf;

        // compute s = f x up (x means "cross product")
        float sx = fy * upZ - fz * upY;
        float sy = fz * upX - fx * upZ;
        float sz = fx * upY - fy * upX;

        // and normalize s
        float rls = 1.0f / length(sx, sy, sz);
        sx *= rls;
        sy *= rls;
        sz *= rls;

        // compute u = s x f
        float ux = sy * fz - sz * fy;
        float uy = sz * fx - sx * fz;
        float uz = sx * fy - sy * fx;

        rm[0] = sx;
        rm[1] = ux;
        rm[2] = -fx;
        rm[3] = 0.0f;

        rm[4] = sy;
        rm[5] = uy;
        rm[6] = -fy;
        rm[7] = 0.0f;

        rm[8] = sz;
        rm[9] = uz;
        rm[10] = -fz;
        rm[11] = 0.0f;

        rm[12] = 0.0f;
        rm[13] = 0.0f;
        rm[14] = 0.0f;
        rm[15] = 1.0f;

        translateM(rm, -eyeX, -eyeY, -eyeZ);
    }

    public static void multiplyMM(float[] result,
                                  float[] lhs, float[] rhs) {

        for (int i = 0; i < 4; i++) {
            result[i*4] = lhs[i*4] * rhs[0] + lhs[i*4+1] *
                rhs[4] + lhs[i*4+2] * rhs[8] + lhs[i*4+3] * rhs[12];

            result[i*4+1] = lhs[i*4] * rhs[1] + lhs[i*4+1] *
                rhs[5] + lhs[i*4+2] * rhs[9] + lhs[i*4+3] * rhs[13];

            result[i*4+2] = lhs[i*4] * rhs[2] + lhs[i*4+1] *
                rhs[6] + lhs[i*4+2] * rhs[10] + lhs[i*4+3] * rhs[14];

            result[i*4+3] = lhs[i*4] * rhs[3] + lhs[i*4+1] *
                rhs[7] + lhs[i*4+2] * rhs[11] + lhs[i*4+3] * rhs[15];
        }
    }

    public static void multiplyMV(float[] resultVec,
                                  float[] lhsMat,
                                  float[] rhsVec) {

        for (int i = 0; i < 4; i++) {
            resultVec[i] =
                lhsMat[i  ] * rhsVec[0] +
                lhsMat[i+4*1] * rhsVec[1] +
                lhsMat[i+4*2] * rhsVec[2] +
                lhsMat[i+4*3] * rhsVec[3];
        }
    }

    public static void perspectiveM(float[] m,
                                    float fovy, float aspect, float zNear, float zFar) {
        float f = 1.0f / (float) Math.tan(fovy * (Math.PI / 360.0));
        float rangeReciprocal = 1.0f / (zNear - zFar);

        m[0] = f / aspect;
        m[1] = 0.0f;
        m[2] = 0.0f;
        m[3] = 0.0f;

        m[4] = 0.0f;
        m[5] = f;
        m[6] = 0.0f;
        m[7] = 0.0f;

        m[8] = 0.0f;
        m[9] = 0.0f;
        m[10] = (zFar + zNear) * rangeReciprocal;
        m[11] = -1.0f;

        m[12] = 0.0f;
        m[13] = 0.0f;
        m[14] = 2.0f * zFar * zNear * rangeReciprocal;
        m[15] = 0.0f;
    }

    public static boolean invertM(float[] mInv, float[] m) {
        // Invert a 4 x 4 matrix using Cramer's Rule

        // transpose matrix
        final float src0  = m[ 0];
        final float src4  = m[ 1];
        final float src8  = m[ 2];
        final float src12 = m[ 3];

        final float src1  = m[ 4];
        final float src5  = m[ 5];
        final float src9  = m[ 6];
        final float src13 = m[ 7];

        final float src2  = m[ 8];
        final float src6  = m[ 9];
        final float src10 = m[10];
        final float src14 = m[11];

        final float src3  = m[12];
        final float src7  = m[13];
        final float src11 = m[14];
        final float src15 = m[15];

        // calculate pairs for first 8 elements (cofactors)
        final float atmp0  = src10 * src15;
        final float atmp1  = src11 * src14;
        final float atmp2  = src9  * src15;
        final float atmp3  = src11 * src13;
        final float atmp4  = src9  * src14;
        final float atmp5  = src10 * src13;
        final float atmp6  = src8  * src15;
        final float atmp7  = src11 * src12;
        final float atmp8  = src8  * src14;
        final float atmp9  = src10 * src12;
        final float atmp10 = src8  * src13;
        final float atmp11 = src9  * src12;

        // calculate first 8 elements (cofactors)
        final float dst0  = (atmp0 * src5 + atmp3 * src6 + atmp4  * src7)
            - (atmp1 * src5 + atmp2 * src6 + atmp5  * src7);
        final float dst1  = (atmp1 * src4 + atmp6 * src6 + atmp9  * src7)
            - (atmp0 * src4 + atmp7 * src6 + atmp8  * src7);
        final float dst2  = (atmp2 * src4 + atmp7 * src5 + atmp10 * src7)
            - (atmp3 * src4 + atmp6 * src5 + atmp11 * src7);
        final float dst3  = (atmp5 * src4 + atmp8 * src5 + atmp11 * src6)
            - (atmp4 * src4 + atmp9 * src5 + atmp10 * src6);
        final float dst4  = (atmp1 * src1 + atmp2 * src2 + atmp5  * src3)
            - (atmp0 * src1 + atmp3 * src2 + atmp4  * src3);
        final float dst5  = (atmp0 * src0 + atmp7 * src2 + atmp8  * src3)
            - (atmp1 * src0 + atmp6 * src2 + atmp9  * src3);
        final float dst6  = (atmp3 * src0 + atmp6 * src1 + atmp11 * src3)
            - (atmp2 * src0 + atmp7 * src1 + atmp10 * src3);
        final float dst7  = (atmp4 * src0 + atmp9 * src1 + atmp10 * src2)
            - (atmp5 * src0 + atmp8 * src1 + atmp11 * src2);

        // calculate pairs for second 8 elements (cofactors)
        final float btmp0  = src2 * src7;
        final float btmp1  = src3 * src6;
        final float btmp2  = src1 * src7;
        final float btmp3  = src3 * src5;
        final float btmp4  = src1 * src6;
        final float btmp5  = src2 * src5;
        final float btmp6  = src0 * src7;
        final float btmp7  = src3 * src4;
        final float btmp8  = src0 * src6;
        final float btmp9  = src2 * src4;
        final float btmp10 = src0 * src5;
        final float btmp11 = src1 * src4;

        // calculate second 8 elements (cofactors)
        final float dst8  = (btmp0  * src13 + btmp3  * src14 + btmp4  * src15)
            - (btmp1  * src13 + btmp2  * src14 + btmp5  * src15);
        final float dst9  = (btmp1  * src12 + btmp6  * src14 + btmp9  * src15)
            - (btmp0  * src12 + btmp7  * src14 + btmp8  * src15);
        final float dst10 = (btmp2  * src12 + btmp7  * src13 + btmp10 * src15)
            - (btmp3  * src12 + btmp6  * src13 + btmp11 * src15);
        final float dst11 = (btmp5  * src12 + btmp8  * src13 + btmp11 * src14)
            - (btmp4  * src12 + btmp9  * src13 + btmp10 * src14);
        final float dst12 = (btmp2  * src10 + btmp5  * src11 + btmp1  * src9 )
            - (btmp4  * src11 + btmp0  * src9  + btmp3  * src10);
        final float dst13 = (btmp8  * src11 + btmp0  * src8  + btmp7  * src10)
            - (btmp6  * src10 + btmp9  * src11 + btmp1  * src8 );
        final float dst14 = (btmp6  * src9  + btmp11 * src11 + btmp3  * src8 )
            - (btmp10 * src11 + btmp2  * src8  + btmp7  * src9 );
        final float dst15 = (btmp10 * src10 + btmp4  * src8  + btmp9  * src9 )
            - (btmp8  * src9  + btmp11 * src10 + btmp5  * src8 );

        // calculate determinant
        final float det =
            src0 * dst0 + src1 * dst1 + src2 * dst2 + src3 * dst3;

        if (det == 0.0f) {
            return false;
        }

        // calculate matrix inverse
        final float invdet = 1.0f / det;
        mInv[ 0] = dst0  * invdet;
        mInv[ 1] = dst1  * invdet;
        mInv[ 2] = dst2  * invdet;
        mInv[ 3] = dst3  * invdet;

        mInv[ 4] = dst4  * invdet;
        mInv[ 5] = dst5  * invdet;
        mInv[ 6] = dst6  * invdet;
        mInv[ 7] = dst7  * invdet;

        mInv[ 8] = dst8  * invdet;
        mInv[ 9] = dst9  * invdet;
        mInv[10] = dst10 * invdet;
        mInv[11] = dst11 * invdet;

        mInv[12] = dst12 * invdet;
        mInv[13] = dst13 * invdet;
        mInv[14] = dst14 * invdet;
        mInv[15] = dst15 * invdet;

        return true;
    }

    public static void transposeM(float[] mTrans, float[] m) {
        for (int i = 0; i < 4; i++) {
            int mBase = i * 4;
            mTrans[i] = m[mBase];
            mTrans[i + 4] = m[mBase + 1];
            mTrans[i + 8] = m[mBase + 2];
            mTrans[i + 12] = m[mBase + 3];
        }
    }
}
