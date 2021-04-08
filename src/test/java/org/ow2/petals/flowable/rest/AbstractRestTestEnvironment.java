/**
 * Copyright (c) 2017-2021 Linagora
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

import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.Invocation;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;

import org.glassfish.jersey.client.authentication.HttpAuthenticationFeature;
import org.junit.Rule;
import org.junit.rules.ExpectedException;
import org.ow2.petals.flowable.AbstractTestEnvironment;
import org.ow2.petals.flowable.FlowableSEConstants;

/**
 * @author Victor NOEL - Linagora
 */
public abstract class AbstractRestTestEnvironment extends AbstractTestEnvironment {

    @Rule
    public final ExpectedException expected = ExpectedException.none();

    protected Client client(final String user, final String password) {
        final ClientBuilder builder = ClientBuilder.newBuilder();
        if (user != null || password != null) {
            builder.register(HttpAuthenticationFeature.basic(user, password));
        }
        return builder.build();
    }

    protected WebTarget target(final String user, final String password, final int port) {
        return client(user, password)
                .target("http://" + FlowableSEConstants.DEFAULT_ENGINE_REST_API_ADDRESS + ":" + port);
    }

    protected Invocation.Builder deployments(final String user, final String password) {
        return deployments(user, password, FlowableSEConstants.DEFAULT_ENGINE_REST_API_PORT);
    }

    protected Invocation.Builder deployments(final String user, final String password, final int port) {
        return target(user, password, port).path("flowable-rest-api/repository/deployments")
                .request(MediaType.APPLICATION_JSON_TYPE);
    }
}
