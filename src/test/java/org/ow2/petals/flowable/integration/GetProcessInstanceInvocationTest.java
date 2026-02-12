/**
 * Copyright (c) 2015-2026 Linagora
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
package org.ow2.petals.flowable.integration;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_OP_GETPROCESSINSTANCES;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_PROCESSINSTANCES_PORT_TYPE;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_PROCESSINSTANCES_SERVICE;

import org.junit.jupiter.api.Test;
import org.ow2.petals.components.flowable.generic._1.GetProcessInstances;
import org.ow2.petals.components.flowable.generic._1.GetProcessInstancesResponse;
import org.ow2.petals.flowable.incoming.integration.GetProcessInstancesOperation;

/**
 * Unit tests about request processing of the service {@link GetProcessInstancesOperation}
 * 
 * @author Christophe DENEUX - Linagora
 */
public class GetProcessInstanceInvocationTest extends AbstractIntegrationServiceInvokations {

    /**
     * <p>
     * Check the processing of the integration service {@link GetProcessInstancesOperation} when:
     * </p>
     * <ul>
     * <li>an invalid request is sent,</li>
     * <li>the request content is not compliant to the XML schema defined in WSDL</li>
     * </ul>
     * <p>
     * Expected results:
     * </p>
     * <ul>
     * <li>no error occurs</li>
     * <li>a fault occurs about the invalid request</li>
     * </ul>
     */
    @Test
    public void invalidRequest_WsdlUncompliant() throws Exception {

        this.testInvalidRequest_WsdlUncompliant(NATIVE_PROCESSINSTANCES_SVC_CFG, ITG_PROCESSINSTANCES_PORT_TYPE,
                ITG_PROCESSINSTANCES_SERVICE, ITG_OP_GETPROCESSINSTANCES);
    }

    /**
     * <p>
     * Check the processing of the integration service {@link GetProcessInstancesOperation} when:
     * </p>
     * <ul>
     * <li>an invalid request is sent,</li>
     * <li>the request content is compliant to the XML schema defined in WSDL</li>
     * </ul>
     * <p>
     * Expected results:
     * </p>
     * <ul>
     * <li>no error occurs</li>
     * <li>a fault occurs about the invalid request</li>
     * </ul>
     */
    @Test
    public void invalidRequest_WsdlCompliant() throws Exception {

        // We use a response as request
        this.testInvalidRequest_WsdlCompliant(NATIVE_PROCESSINSTANCES_SVC_CFG, ITG_PROCESSINSTANCES_PORT_TYPE,
                ITG_PROCESSINSTANCES_SERVICE, ITG_OP_GETPROCESSINSTANCES, new GetProcessInstancesResponse());
    }

    /**
     * <p>
     * Check the processing of the integration service {@link GetProcessInstancesOperation} when:
     * </p>
     * <ul>
     * <li>an empty request is sent</li>
     * </ul>
     * <p>
     * Expected results:
     * </p>
     * <ul>
     * <li>no error occurs</li>
     * <li>a fault occurs about the invalid request</li>
     * </ul>
     */
    @Test
    public void invalidRequest_EmptyRequest() throws Exception {

        this.testInvalidRequest_Empty(NATIVE_PROCESSINSTANCES_SVC_CFG, ITG_PROCESSINSTANCES_PORT_TYPE,
                ITG_PROCESSINSTANCES_SERVICE, ITG_OP_GETPROCESSINSTANCES);
    }

    /**
     * <p>
     * Check the processing of the integration service {@link GetProcessInstancesOperation} when:
     * </p>
     * <ul>
     * <li>no argument is given</li>
     * </ul>
     * <p>
     * Expected results:
     * </p>
     * <ul>
     * <li>no error occurs</li>
     * <li>no fault occurs</li>
     * <li>no task returns because no process instance exists</li>
     * </ul>
     */
    @Test
    public void noArguments() throws Exception {

        final Object getProcessInstancesRespObj = this.testRequest(NATIVE_PROCESSINSTANCES_SVC_CFG,
                ITG_PROCESSINSTANCES_PORT_TYPE, ITG_PROCESSINSTANCES_SERVICE, ITG_OP_GETPROCESSINSTANCES,
                new GetProcessInstances());

        assertInstanceOf(GetProcessInstancesResponse.class, getProcessInstancesRespObj);
        final GetProcessInstancesResponse getProcessInstancesResp = (GetProcessInstancesResponse) getProcessInstancesRespObj;
        assertNotNull(getProcessInstancesResp.getProcessInstances());
        assertNotNull(getProcessInstancesResp.getProcessInstances().getProcessInstance());
        assertEquals(0, getProcessInstancesResp.getProcessInstances().getProcessInstance().size());
    }

}
