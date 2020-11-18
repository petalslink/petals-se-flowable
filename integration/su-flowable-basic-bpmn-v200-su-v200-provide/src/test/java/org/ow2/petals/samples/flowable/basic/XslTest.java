/**
 * Copyright (c) 2020 Linagora
 *
 * This program/library is free software: you can redistribute it and/or modify
 * it under the terms of the New BSD License (3-clause license).
 *
 * This program/library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the New BSD License (3-clause license)
 * for more details.
 *
 * You should have received a copy of the New BSD License (3-clause license)
 * along with this program/library; If not, see http://directory.fsf.org/wiki/License:BSD_3Clause/
 * for the New BSD License (3-clause license).
 */
package org.ow2.petals.samples.flowable.basic;

import static org.ow2.petals.flowable.junit.Assert.assertXslTransformation;

import java.io.IOException;

import javax.xml.transform.TransformerException;

import org.junit.Test;
import org.xml.sax.SAXException;

/**
 * Unit tests about XSL transformations embedded into the SU
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public class XslTest {

    private static final String XML_RESULT_DIR = "xml-result/";

    private static final String STARTBASICPROCESS_DIR = "startBasicProcess/";

    private static final String STARTBASICPROCESS_RESULT_DIR = XML_RESULT_DIR + STARTBASICPROCESS_DIR;

    private static final String COMPLETE_DIR = "complete/";

    private static final String COMPLETE_RESULT_DIR = XML_RESULT_DIR + COMPLETE_DIR;

    private static final String XSL_STARTBASICPROCESS = "startBasicProcessResponse.xsl";

    private static final String XSL_COMPLETE = "completeResponse.xsl";

    /**
     * Check the XSL 'startBasicProcessResponse.xsl'
     */
    @Test
    public void startMasterProcessResponse_Nominal() throws IOException, TransformerException, SAXException {
        assertXslTransformation(STARTBASICPROCESS_RESULT_DIR + "nominal.xml", XSL_STARTBASICPROCESS, "AZE123", null,
                null, false);
    }

    /**
     * Check the XSL 'completeResponse.xsl'
     */
    @Test
    public void completeResponse_Nominal() throws IOException, TransformerException, SAXException {
        assertXslTransformation(COMPLETE_RESULT_DIR + "nominal.xml", XSL_COMPLETE, "AZE123", null, null, false);
    }

}
