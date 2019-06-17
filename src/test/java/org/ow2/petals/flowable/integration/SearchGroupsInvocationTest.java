/**
 * Copyright (c) 2017-2019 Linagora
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

import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_GROUP_PORT_TYPE;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_GROUP_SERVICE;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_OP_SEARCHGROUPS;

import org.junit.Test;
import org.ow2.petals.components.flowable.generic._1.SearchGroups;
import org.ow2.petals.components.flowable.generic._1.SearchGroupsResponse;
import org.ow2.petals.flowable.incoming.integration.SearchGroupsOperation;

/**
 * Unit tests about request processing of the service {@link SearchGroupsOperation}
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class SearchGroupsInvocationTest extends AbstractIntegrationServiceInvokations {

    /**
     * <p>
     * Check the processing of the integration service {@link SearchGroupsOperation} when:
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
    public void invalidRequest_WsdlUncompliantRequest() throws Exception {

        this.testInvalidRequest_WsdlUncompliant(NATIVE_GROUP_SVC_CFG, ITG_GROUP_PORT_TYPE, ITG_GROUP_SERVICE,
                ITG_OP_SEARCHGROUPS);
    }

    /**
     * <p>
     * Check the processing of the integration service {@link SearchGroupsOperation} when:
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
    public void invalidRequest_WsdlCompliantRequest() throws Exception {

        // We use a response as request
        final SearchGroupsResponse request = new SearchGroupsResponse();
        request.getGroupId().add("group-id");
        this.testInvalidRequest_WsdlCompliant(NATIVE_GROUP_SVC_CFG, ITG_GROUP_PORT_TYPE, ITG_GROUP_SERVICE,
                ITG_OP_SEARCHGROUPS, request);
    }

    /**
     * <p>
     * Check the processing of the integration service {@link SearchGroupsOperation} when:
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

        this.testInvalidRequest_Empty(NATIVE_GROUP_SVC_CFG, ITG_GROUP_PORT_TYPE, ITG_GROUP_SERVICE,
                ITG_OP_SEARCHGROUPS);
    }

    /**
     * <p>
     * Check the processing of the integration service {@link SearchGroupsOperation} when:
     * </p>
     * <ul>
     * <li>valid arguments are given to retrieve all users of a given group</li>
     * </ul>
     * <p>
     * Expected results:
     * </p>
     * <ul>
     * <li>no error occurs</li>
     * <li>user information are correctly retrieved</li>
     * </ul>
     */
    @Test
    public void validRequest() throws Exception {

        final SearchGroups bean = new SearchGroups();
        bean.setUserId(BPMN_USER_VALIDEUR);
        final Object SearchGroupsRespObj = this.testRequest(NATIVE_GROUP_SVC_CFG, ITG_GROUP_PORT_TYPE,
                ITG_GROUP_SERVICE,
                ITG_OP_SEARCHGROUPS, bean);

        assertTrue(SearchGroupsRespObj instanceof SearchGroupsResponse);
        assertNotNull(((SearchGroupsResponse) SearchGroupsRespObj).getGroupId());
        assertEquals(1, ((SearchGroupsResponse) SearchGroupsRespObj).getGroupId().size());
        assertEquals("management", ((SearchGroupsResponse) SearchGroupsRespObj).getGroupId().get(0));
    }
}
