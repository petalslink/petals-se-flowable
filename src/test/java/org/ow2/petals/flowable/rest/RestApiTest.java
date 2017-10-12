/**
 * Copyright (c) 2017 Linagora
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
package org.ow2.petals.flowable.rest;

import java.net.URL;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import javax.xml.namespace.QName;

import org.junit.ClassRule;
import org.junit.Test;
import org.junit.rules.RuleChain;
import org.junit.rules.TestRule;
import org.ow2.petals.component.framework.junit.impl.ProvidesServiceConfiguration;
import org.ow2.petals.component.framework.junit.impl.ServiceConfiguration;
import org.ow2.petals.component.framework.junit.rule.ComponentUnderTest;
import org.ow2.petals.component.framework.junit.rule.ServiceConfigurationFactory;
import org.ow2.petals.flowable.FlowableSEConstants;

import com.fasterxml.jackson.databind.JsonNode;

/**
 * Class for unit tests about the use of the REST API
 * 
 * @author Victor NOEL - Linagora
 * @author Jordy CABANNES - Linagora
 */
public class RestApiTest extends AbstractRestTestEnvironment {

    protected static final String INONLY_SU = "in-only";

    private static final String INONLY_NAMESPACE = "http://petals.ow2.org/se-flowable/unit-test/in-only";

    protected static final QName INONLY_INTERFACE = new QName(INONLY_NAMESPACE, "in-only");

    protected static final QName INONLY_SERVICE = new QName(INONLY_NAMESPACE, "in-only-service");

    protected static final String INONLY_ENDPOINT = "edpInOnly";

    protected static final ComponentUnderTest COMPONENT_UNDER_TEST = new ComponentUnderTest()
            .addLogHandler(IN_MEMORY_LOG_HANDLER.getHandler())
            .registerServiceToDeploy(INONLY_SU, new ServiceConfigurationFactory() {
                @Override
                public ServiceConfiguration create() {

                    final URL wsdlUrl = Thread.currentThread().getContextClassLoader()
                            .getResource("su/in-only/in-only.wsdl");
                    assertNotNull("WSDL not found", wsdlUrl);
                    final ProvidesServiceConfiguration serviceConfiguration = new ProvidesServiceConfiguration(
                            INONLY_INTERFACE, INONLY_SERVICE, INONLY_ENDPOINT, wsdlUrl);

                    final URL startResponseXslUrl = Thread.currentThread().getContextClassLoader()
                            .getResource("su/in-only/startResponse.xsl");
                    assertNotNull("Output XSL 'startResponse.xsl' not found", startResponseXslUrl);
                    serviceConfiguration.addResource(startResponseXslUrl);

                    final URL bpmnUrl = Thread.currentThread().getContextClassLoader()
                            .getResource("su/in-only/in-only.bpmn");
                    assertNotNull("BPMN file not found", bpmnUrl);
                    serviceConfiguration.addResource(bpmnUrl);

                    serviceConfiguration.setServicesSectionParameter(
                            new QName(FlowableSEConstants.NAMESPACE_SU, "process_file"), "in-only.bpmn");
                    serviceConfiguration
                            .setServicesSectionParameter(new QName(FlowableSEConstants.NAMESPACE_SU, "version"), "1");

                    return serviceConfiguration;
                }
            });

    @ClassRule
    public static final TestRule chain = RuleChain.outerRule(TEMP_FOLDER).around(IN_MEMORY_LOG_HANDLER)
            .around(COMPONENT_UNDER_TEST);

    @Override
    protected ComponentUnderTest getComponentUnderTest() {
        return COMPONENT_UNDER_TEST;
    }

    /**
     * Check that we cannot access to REST API without login/password.
     */
    @Test
    public void getOnApiNoLogin() {
        final Response response = deployments(null, null).get();

        assertEquals(Status.UNAUTHORIZED.getStatusCode(), response.getStatus());
    }

    /**
     * Check that we cannot access to REST API with a bad login/password.
     */
    @Test
    public void getOnApiBadLogin() {
        final Response response = deployments("rest-api-user", "lololol").get();

        assertEquals(Status.UNAUTHORIZED.getStatusCode(), response.getStatus());
    }

    /**
     * Check that we cannot access to REST API with login/password of a user who does not belong to the
     * ENGINE_REST_API_ACCESS_GROUP.
     */
    @Test
    public void getOnApiIncorrectLogin() {
        final Response response = deployments("fozzie", "fozzie").get();

        assertEquals(Status.FORBIDDEN.getStatusCode(), response.getStatus());
    }

    /**
     * Check that we can access to REST API with login/password of a user who belongs to the
     * ENGINE_REST_API_ACCESS_GROUP.
     */
    @Test
    public void getOnApiCorrectLogin() {
        final Response response = deployments("rest-api-user", "user-api-rest-password").get();

        assertEquals(Status.OK.getStatusCode(), response.getStatus());
    }

    /**
     * Check that we retrieve correct data through the REST API.
     * 
     * Expected result: the process that was instantiated in this class
     */
    @Test
    public void hasDeployments() {
        final JsonNode response = deployments("rest-api-user", "user-api-rest-password").get(JsonNode.class);

        final JsonNode jsonNode = response.get("data");
        assertTrue(jsonNode.isArray());
        assertEquals(1, jsonNode.size());
        assertEquals("Process read from: in-only.bpmn", jsonNode.get(0).get("name").asText());
    }
}
