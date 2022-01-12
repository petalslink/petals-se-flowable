/**
 * Copyright (c) 2020-2022 Linagora
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

import static org.ow2.petals.flowable.junit.Assert.assertWsdlCompliance;

import javax.xml.namespace.QName;

import org.junit.Test;

/**
 * Unit test of the compliance between service WSDL and process definition
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public class WsdlComplianceTest {

    private static final String NAMESPACE_SVC = "http://petals.ow2.org/integration/tests/se-flowable/basic/services/v1";

    @Test
    public void validate() throws Exception {
        assertWsdlCompliance(
                new QName[] { new QName(NAMESPACE_SVC, "startBasicProcess"), new QName(NAMESPACE_SVC, "complete") });
    }

}
