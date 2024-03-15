/**
 * Copyright (c) 2018-2024 Linagora
 * 
 * This program/library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or (at your
 * option) any later version.
 * 
 * This program/library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program/library; If not, see http://www.gnu.org/licenses/
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.samples.flowable.collaboration;

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

    private static final String NAMESPACE_SVC = "http://petals.ow2.org/samples/se-flowable/collaboration/services";

    @Test
    public void validate() throws Exception {
        assertWsdlCompliance(
                new QName[] { new QName(NAMESPACE_SVC, "startMasterProcess"),
                        new QName(NAMESPACE_SVC, "startChildProcess"),
                        new QName(NAMESPACE_SVC, "notifyMasterProcess") });
    }

}
